library(tidyverse)
library(fs)
library(gganimate)
# library(ggtext)
library(scales)
library(marquee)

# Read in data
data_path <- dir_ls("2025/20251204_hus_pris_antal/data/clean/")
data <- map(data_path, read_delim) |> 
  setNames(str_extract(data_path, "\\w*(?=\\.txt)"))

# Join data
joined_data <- left_join(data$utbud, data$bostadsratter_pris) |> 
  # Add in the price data for Småhus
  rows_update(
    data$smahus_pris |> select(Kommun, År, Hustyp, Pris),
    by = c("Kommun", "År", "Hustyp"), 
    unmatched = "ignore"
  ) |> 
  # Force start year from 1996 (complete data)
  filter(År >= 1996) |> 
  # Calculate percent increase over start year
  mutate(
    price_index = Pris / first(Pris),
    antal_index = Antal / first(Antal), 
    .by = c(Kommun, Hustyp))

# Calculate the scaling factor based on max values
max_smahus <- joined_data |> 
  filter(Hustyp == "SMÅHUS") |> 
  pull(Pris) |> 
  max(na.rm = TRUE)

max_flerbost <- joined_data |> 
  filter(Hustyp == "FLERBOST") |> 
  pull(Pris) |> 
  max(na.rm = TRUE)

scale_factor <- max_smahus / max_flerbost

# Plot
p <- ggplot() +
    # Labels
  labs(
    subtitle = "Samhället behöver fungerande marknader, vilket ytterst är politikens ansvar att rå för. I våra våra största kommuner har priserna stuckit iväg 
       med flera hundra procent, utan att nödväntigtvis följa utbud. Fungerar en sådan 
       marknad som den utvecklats under de senaste 30 åren? Hur ska vi får fart på prisvärda 
       småhus givet dess prisutveckling och en fabläss för att bygga bostadsrätter?",
    caption = "**Data**: SCB & Svensk Mäklarstatistik | **R packages**: {tidyverse, fs, gganimate, ggtext, scales} | **Code**: github.com/hugoakerstrand/swedish_politics",
    tag = "{round(frame_along)}"
  ) +
    # Points
    geom_line(data = filter(joined_data, Hustyp == "SMÅHUS"), 
               aes(x = Antal, y = Pris, color = Kommun), size = 0.75) +
    geom_line(data = filter(joined_data, Hustyp == "FLERBOST"), 
               aes(x = Antal, y = Pris * scale_factor, color = Kommun),
               size = 0.75) +
    geom_point(data = filter(joined_data, Hustyp == "SMÅHUS"), 
               aes(x = Antal, y = Pris, color = Kommun, group = seq_along(År)), 
               size = 0.75) +
    geom_point(data = filter(joined_data, Hustyp == "FLERBOST"), 
               aes(x = Antal, y = Pris * scale_factor, color = Kommun, group = seq_along(År)), 
               size = 0.75) +
    # Double y axis
    scale_y_continuous(
      labels = label_number(scale_cut = cut_short_scale()),
      name = "Värdering (Småhus)",
      sec.axis = sec_axis(~ . / scale_factor, name = "Värdering (Bostadsrätt, kr/m<sup>2</sup>)", labels = label_number(scale_cut = cut_short_scale()))
    ) +
    # Wrap by Kommun
    # facet_wrap(~ Hustyp, scales = "free", labeller = as_labeller(c(
      # "SMÅHUS" = "Småhus (kr)",
      # "FLERBOST" = "Bostadsrätt (kr/m<sup>2</sup>)"
    # ))) +
    # Large year text in background
    ggtitle("**En fungerande bostadsmarknad?**") +
    # Y scale
    # scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), name = "Genomsnittlig värdering i kommun") +
    scale_x_continuous(labels = label_number(big.mark = " "), name = "Antal bostäder") +
    # Theme
    theme(
      plot.title = element_markdown(size = 7),
      plot.subtitle = element_textbox(size = 5),
      strip.text = element_markdown(size = 5),
      plot.caption = element_markdown(size = 4),
      axis.text = element_text(size = 4.5),
      axis.title.y.right = element_markdown(size = 4.5),
      axis.title = element_text(size = 5),
      legend.position = "none",
      plot.tag = element_markdown(hjust = 0, size = 15, face = "bold", color = "grey75"),
      plot.tag.position = c(0.45, 0.5)
    ) +
  # Animate by year 
  transition_reveal(År) 

p <- ggplot() +
  # Labels
  labs(
    subtitle = "Samhället behöver fungerande marknader, vilket ytterst är politikens ansvar att rå för. I våra våra största kommuner har priserna stuckit iväg 
       med flera hundra procent, utan att nödväntigtvis följa utbud. Fungerar en sådan 
       marknad som den utvecklats under de senaste 30 åren? Hur ska vi får fart på prisvärda 
       småhus givet dess prisutveckling och en fabläss för att bygga bostadsrätter?",
    caption = "**Data**: SCB & Svensk Mäklarstatistik | **R packages**: {tidyverse, fs, gganimate, marquee, scales} | **Code**: github.com/hugoakerstrand/swedish_politics",
    tag = "{round(frame_along)}"
  ) +
  # Points
  geom_line(data = filter(joined_data, Hustyp == "SMÅHUS"), 
            aes(x = Antal, y = Pris, color = Kommun), size = 0.75) +
  geom_line(data = filter(joined_data, Hustyp == "FLERBOST"), 
            aes(x = Antal, y = Pris * scale_factor, color = Kommun),
            size = 0.75) +
  geom_point(data = filter(joined_data, Hustyp == "SMÅHUS"), 
             aes(x = Antal, y = Pris, color = Kommun, group = seq_along(År)), 
             size = 0.75) +
  geom_point(data = filter(joined_data, Hustyp == "FLERBOST"), 
             aes(x = Antal, y = Pris * scale_factor, color = Kommun, group = seq_along(År)), 
             size = 0.75) +
  # Double y axis
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale()),
    name = "Värdering (Småhus)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Värdering (Bostadsrätt, kr/m<sup>2</sup>)", labels = label_number(scale_cut = cut_short_scale()))
  ) +
  # Wrap by Kommun
  # facet_wrap(~ Hustyp, scales = "free", labeller = as_labeller(c(
  # "SMÅHUS" = "Småhus (kr)",
  # "FLERBOST" = "Bostadsrätt (kr/m<sup>2</sup>)"
  # ))) +
  # Large year text in background
  ggtitle("**En fungerande bostadsmarknad?**") +
  # Y scale
  # scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), name = "Genomsnittlig värdering i kommun") +
  scale_x_continuous(labels = label_number(big.mark = " "), name = "Antal bostäder") +
  # Theme
  theme(
    plot.title = element_marquee(size = 7),
    plot.subtitle = element_marquee(size = 5),
    strip.text = element_marquee(size = 5),
    plot.caption = element_marquee(size = 4),
    axis.text = element_text(size = 4.5),
    axis.title.y.right = element_marquee(size = 4.5),
    axis.title = element_text(size = 5),
    legend.position = "none",
    plot.tag = element_text(hjust = 0, size = 15, face = "bold", color = "grey75"),
    plot.tag.position = c(0.45, 0.5)
  )  +
  # Animate by year 
  transition_reveal(År)

# Animate
animate(p, width = 10, height = 5, units = "cm", res = 285, end_pause = 15)
