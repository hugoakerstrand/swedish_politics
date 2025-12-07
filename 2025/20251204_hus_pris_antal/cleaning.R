# Paste code used to create the curated dataset here. Include comments as
# necessary. If you did not need to clean the data, use a comment like the one
# below, but also load the data with readr::read_csv() to ensure the data can be
# loaded, and to use with `saving.R`. Delete this block of comments.

# Clean data provided by <source of data>. No cleaning was necessary.
library(tidyverse)
library(httr2)

# 
table_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0501/BO0501B/FastprisSHRegionAr"

query_json_str <- '{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "vs:RegionKommun07EjAggr",
        "values": ["0180", "1280", "1480"]
      }
    },
    {
      "code": "Fastighetstyp",
      "selection": {
        "filter": "item",
        "values": ["220"]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": ["BO0501C3"]
      }
    }
  ],
  "response": {
    "format": "json"
  }
}'

# POST-förfrågan till SCB:s API
response <- request(table_url) |> 
  req_method("POST") |> 
  req_body_raw(query_json_str, type = "application/json") |> 
  req_perform()

content <- resp_body_json(response)

# Extract column names
col_names <- content$columns |> map_chr("text")

# Convert to tibble
df_1 <- content$data |>
  map_dfr(\(row) {
    c(row$key, row$values) |>
      set_names(col_names) |>
      as_tibble()
  }) |>
  mutate(
    år = as.integer(år),
    across(last_col(), as.numeric),
    kommun = case_when(
      region == "0180" ~ "Stockholm",
      region == "1280" ~ "Malmö",
      region == "1480" ~ "Göteborg",
      .default = region
    )
  )

# Second data frame - BO0104T04
table_url_2 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0104/BO0104D/BO0104T04"

query_json_str_2 <- '{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "vs:RegionKommun07",
        "values": [
          "0180",
          "1280",
          "1480"
        ]
      }
    },
    {
      "code": "Hustyp",
      "selection": {
        "filter": "item",
        "values": [
          "SMÅHUS",
          "FLERBOST"
        ]
      }
    },
    {
      "code": "Upplatelseform",
      "selection": {
        "filter": "item",
        "values": [
          "2",
          "3"
        ]
      }
    }
  ],
  "response": {
    "format": "json"
  }
}'

# POST-förfrågan till SCB:s API
response_2 <- request(table_url_2) |>
  req_method("POST") |>
  req_body_raw(query_json_str_2, type = "application/json") |>
  req_perform()

content_2 <- resp_body_json(response_2)

# Extract column names
col_names_2 <- content_2$columns |> map_chr("text")

# Convert to tibble
df_2 <- content_2$data |>
  map_dfr(\(row) {
    c(row$key, row$values) |>
      set_names(col_names_2) |>
      as_tibble()
  }) |>
  mutate(
    år = as.integer(år),
    across(last_col(), as.numeric),
    kommun = case_when(
      region == "0180" ~ "Stockholm",
      region == "1280" ~ "Malmö",
      region == "1480" ~ "Göteborg",
      .default = region
    )
  )
