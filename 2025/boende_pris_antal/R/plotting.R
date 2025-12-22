library(tidyverse)
library(gganimate)
library(fs)
library(ggtext)
library(showtext)
library(scales)

# Fonts
font_add_google("DM Serif Text", "DM")
showtext_auto()
showtext_opts(dpi = 285)

body_font <- "DM"
title_font <- "DM"

# Read in data
data <- dir_ls("2025/20251204_hus_pris_antal/data/clean/") |> read_rds()

# Subset to complete data (i.e. rows == 29, 1996-2024)
complete_brf_data <- data$Prices_brf |> 
  map_lgl(function(brf_data) {
  row <- ifelse(nrow(brf_data) |> is.null(), 0, nrow(brf_data))
  row == 29
}) 

data <- data |> filter(complete_brf_data) |> drop_na(Kommun)

# Separate objects by `Hustyp`
hustyp_id <- c("småhus", "flerbostadshus")

# Create object to plot data from: combining `utbud` (availability) and 
# `pris` (cost)
plot_data <- map(hustyp_id, function(hustyp_id) {
  
  # Availability data from SCB
  utbud_data <- data$Utbud |> 
    bind_rows() |> 
    filter(Hustyp == hustyp_id) |> 
    select(Kommun, År, Antal)
  
  # Price data from SCB (`småhus`) else Svensk mäklarstatistik
  price_data <- if(hustyp_id == "småhus") {
    data$Price_smahus |> 
      bind_rows()
    
  } else {
    data$Prices_brf |>
      set_names(data$Kommun) |> 
      bind_rows(.id = "Kommun") |> 
      rename(År = ar, Pris = kr_per_kvm)
  }
  
  # Join data into single object
  left_join(utbud_data, price_data, by = join_by(Kommun, År))
  
}) |> 
  setNames(hustyp_id) |> 
  bind_rows(.id = "Hustyp") |> 
  
  # Add back information about region (`Län`)
  left_join(select(data, Lan, Kommun), by = join_by(Kommun)) |> 
  
  # Remove observations before 1996
  filter(År >= 1996)

# Plot data

plot <- plot_data |> 
  
  # Initiate ggplot object
  ggplot() + 
  
  # Labels
  labs(
    title = "Utbud och pris för flerbostäder och småhus år {floor(frame_time)}", 
    subtitle = "Bostadsbyggandet de senaste 30 åren har dominerats av 
      flerbostäder (bostadsrätter) i Stockholm; småhus har i princip inte byggts.  
      Samtidigt har priserna ökat för småhus och flerbostadshus i hela riket, 
      vilket gör det svårt att komma in på marknaden och skapat  
      stora skillnader i förmögenhet mellan generationerna.
      <br>
      <br>
      <i>**Pris** avser genomsnitt i kronor per kvadratmeter (flerbostadshus) eller 
      taxeringsvärde (småhus).</i>",
    color = "Kommun",
    caption = 
      "**Data:** SCB & Svensk Mäklarstatistik &emsp;| **Code:** github.com/hugoakerstrand/swedish_politics"
  ) +
  
  # Create dot plot layer
  geom_point(aes(x = Antal, y = Pris, color = Kommun, group = Kommun), size = 1.75, alpha = 0.5) +
  
  # Custom scale formatting
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_x_continuous(labels = label_number(big.mark = " "), name = "Antal") +
  
  # Custom theme options
  theme(
    plot.title = element_markdown(size = 11, family = title_font),
    plot.subtitle = element_textbox(
      size = 4.5, 
      width = unit(9.75, "cm"),
      lineheight = 1.3,
      padding = margin(2, 0, 2, 2.1),
      margin = margin(0, 0, 7, 0),
      halign = 0
    ),
    strip.text = element_markdown(size = 5, family = body_font),
    plot.caption = element_textbox(
      size = 4.5, 
      width = unit(15, "cm"),
      lineheight = 1.3,
      padding = margin(0, 0, 0, 0),
      margin = margin(0.2, 0, 0, 5.85, unit = "cm"),
      halign = 0
    ),
    axis.text = element_text(size = 4.5),
    axis.title = element_text(size = 5),
    legend.position = "right",
    legend.text = element_text(size = 4, family = body_font),
    legend.key.size = unit(0.2, "cm"),
    legend.title = element_text(size = 5, family = title_font)
  ) +
  
  # Wrap by house type
  facet_wrap(~ Hustyp, scales = "free_y", labeller = labeller(Hustyp = c(flerbostadshus = "Flerbostadshus", småhus = "Småhus"))) +
  
  # Distribute color guide by rows
  guides(color = guide_legend(nrow = 13)) +
  
  # Animate by year 
  transition_time(År)

animated_plot <- animate(plot, width = 15, height = 7.75, units = "cm", res = 285, end_pause = 15)
animated_plot

plot_path <- path_abs("2025/20251204_hus_pris_antal/pris_hus_antal.gif")
anim_save(plot_path)
