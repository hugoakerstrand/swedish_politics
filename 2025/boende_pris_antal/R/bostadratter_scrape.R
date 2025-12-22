#!/usr/bin/env Rscript
# Mäklarstatistik Scraper - All 290 Swedish Municipalities

# Load packages ----------------------------------------------------------------
library(chromote)
library(rvest)
library(httr2)
library(polite)
library(tidyverse)
library(glue)
library(fs)
library(cli)
library(jsonlite)
source("2025/boende_pris_antal/R/scrape_kommun.R")

target_path <- path("2025/boende_pris_antal/data/raw/")

cli_h1("Mäklarstatistik - All 290 Swedish Municipalities")
cli_alert_info("Using {{polite}} package for respectful web scraping")

# Configuration ----------------------------------------------------------------
TEST_MODE <- FALSE  # Set to TRUE to test with Stockholm only

# Setup ------------------------------------------------------------------------
# Create output directory
fs::dir_create(target_path)

# Step 1: Discover all municipality URLs --------------------------------------
cli_h2("Step 1: Discovering municipalities")

# Introduce ourselves politely to the website
main_url <- "https://www.maklarstatistik.se/"

session <- bow(
  main_url,
  user_agent = "Polite R Web Scraper",
  delay = 5
)

# Get the main page to find all municipality links
page <- scrape(session)

# Extract all municipality (kommun) links
kommun_links <- page |>
  html_elements("a[href*='/omrade/riket/']") |>
  html_attr("href") |>
  unique() |> 
  keep(\(links) str_detect(links, "/omrade/riket/.+\\-lan/.+"))

# Create data frame
kommun_data <- tibble(url = kommun_links) |>
  mutate(
    lan = str_extract(url, "riket/(.+)(?=-lan)", group = 1),
    kommun = str_extract(url, "riket/.+/(\\w+)", group = 1),
    url = str_glue("https://www.maklarstatistik.se/omrade/riket/{lan}/{kommun}/#/bostadsratter/arshistorik-prisutveckling")
  )

cli_alert_success("Found {nrow(kommun_data)} municipalities")

if(nrow(kommun_data) == 0) {
  cli_abort("No municipalities found. Check the website structure.")
}

# TEST MODE: Filter to Stockholm only
if (TEST_MODE) {
  cli_alert_warning("TEST MODE ENABLED")
  cli_alert_info("Filtering to Stockholm only for testing")

  kommun_data <- kommun_data |>
    filter(kommun == "stockholm")

  if (nrow(kommun_data) == 0) {
    cli_abort("Stockholm not found in municipality list")
  }
}

# Step 2: Scrape each municipality ---------------------------------------------
cli_h2("Step 2: Starting scrape")
cli_alert_info("Using {{polite}} package with 5 second delays")
cli_alert_info("Estimated time: ~{ceiling(nrow(kommun_data) * 5 / 60)} minutes")

# Scrape all municipalities using purrr
results <- kommun_data |>
  mutate(
    index = row_number(),
    prices = pmap(list(url, lan, kommun, index), 
                  ~scrape_kommun(..1, ..3, ..4, nrow(kommun_data)))
  )

# Write to rds ----------------------------------------------------------------
saveRDS(results, file = paste0(target_path, "/bostadsratter.rds"))

cli_alert_success("All done!")
