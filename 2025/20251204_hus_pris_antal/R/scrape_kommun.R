# Function to scrape a single municipality
scrape_kommun <- function(url, kommun_name, index, total) {
  cli_alert_info("[{index}/{total}] {kommun_name}")
  
  tryCatch({
    # Start browser
    b <- ChromoteSession$new()
    
    # Navigate to base URL first
    base_url <- str_replace(url, "#.*$", "")
    b$Page$navigate(base_url)
    
    # Wait for SPA to initialize
    Sys.sleep(1)
    
    # Set hash directly to trigger yearly view
    b$Runtime$evaluate("
      window.location.hash = '#/bostadsratter/arshistorik-prisutveckling';
      var event = new HashChangeEvent('hashchange', {
        newURL: window.location.href,
        oldURL: window.location.href.split('#')[0]
      });
      window.dispatchEvent(event);
    ")
    
    # Wait for yearly data to load
    Sys.sleep(1)
    
    # Get fully rendered HTML
    html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    page <- read_html(html)
    
    # Extract yearly historic chart data (from canvas element, not table!)
    arshistorik_canvas <- page |>
      html_elements("canvas[data-chart-name='Årshistorik'][data-chart-category='prisutveckling'][data-tenure-type='br']")
    
    if (length(arshistorik_canvas) == 0) {
      cli_alert_warning("Yearly historic chart not found")
      b$close()
      return(list(success = FALSE, kommun = kommun_name))
    }
    
    # Parse chart data
    labels_json <- arshistorik_canvas |> html_attr("data-labels")
    series_json <- arshistorik_canvas |> html_attr("data-series")
    
    # Decode HTML entities
    labels_json <- gsub("&quot;", '"', labels_json)
    series_json <- gsub("&quot;", '"', series_json)
    
    # Parse JSON
    years <- jsonlite::fromJSON(labels_json) |> as.numeric()
    prices <- jsonlite::fromJSON(series_json) |> as.numeric()
    
    if (length(years) > 0) {
      data <- tibble(
        ar = years,
        kr_per_kvm = prices
      )
      b$close()
      data
    } else {
      cli_alert_warning("No data extracted")
      b$close()
      tibble(
        ar = NA_real_,
        kr_per_kvm = NA_real_
      )
    }
    
  }, error = function(e) {
    cli_alert_danger("Error: {e$message}")
    tibble(
      ar = NA_real_,
      kr_per_kvm = NA_real_
    )
  })
}
