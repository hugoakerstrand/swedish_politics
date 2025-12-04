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

content$data |> 
  map(\(data) tibble(key = key))
