

# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(rvest)
library(rnaturalearth)

# Load data ---------------------------------------------------------------

dhs_countries <-
  read_html(
    "https://dhsprogram.com/methodology/survey-search.cfm?sendsearch=1&crt=1&listgrp=0"
  ) |>
  html_table()

# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Source Sans 3", "source_sans")
font_add(family = "fa",
         regular = "fontawesome/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()


# Define colours ----------------------------------------------------------

greens <- RColorBrewer::brewer.pal(3, "Greens")


# Data wrangling ----------------------------------------------------------

dhs_countries_table <- dhs_countries |>
  # extract table
  purrr::pluck(6) |>
  janitor::row_to_names(1) |>
  janitor::clean_names() |>
  # filter out country headers
  filter(country_year != type) |>
  # get rid of footnotes
  mutate(country_year = str_replace(country_year, "\\([0-9]*\\)", "")) |>
  # split country/year into two variables
  tidyr::separate(country_year,
                  into = c("country", "year_description"),
                  # separate letters/parentheses from years
                  sep = "(?<=[a-zA-Z\\(\\)])\\s*(?=[0-9])") |>
  mutate(start_year = substr(year_description, 1, 4) |> as.integer())

gps_timeseries <-
  dhs_countries_table |>
  filter(type == "Standard DHS") |>
  filter(gps_datasets == "Data Available") |>
  count(country, name = "n_cross_section") |>
  mutate(n_cross_section = case_match(n_cross_section,
                                      1 ~ "1",
                                      2 ~ "2",
                                      3 ~ "3",
                                      4:6 ~ "4+")) |>
  mutate(iso_3 = countrycode::countrycode(country, "country.name", "iso3c"))

world_time_series <- ne_countries(returnclass = "sf") |>
  select(iso_a3_eh, sovereignt) |>
  merge(gps_timeseries,
        by.x = "iso_a3_eh",
        by.y = "iso_3",
        all.x = TRUE)

# ocean
grat <- sf::st_graticule() |> sf::st_cast('POLYGON')


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-01-02", "recording"),
  device = "png",
  width = 1080,
  height = 1920,
  units = "px",
  dpi = 72,
  bg = "white"
)


# Define text -------------------------------------------------------------

# let's go hacky for the caption
tab <- function(tab_width) {
  paste0(
    "<span style='color:transparent;'>",
    rep(".", tab_width) |> paste(collapse = ""),
    "</span>"
  )
}

social <- paste0(
  "<span style='font-family:fa;'>&#xf09b;</span>",
  tab(2),
  "sarahzeller",
  tab(4),
  "<span style='font-family:fa;'>&#xf099;</span>",
  tab(2),
  "@sarah_y_zeller",
  tab(2),
  "| #tidytuesday"
)
title <- "DHS time series"
st <-
  glue::glue(
    "The Demographic and Health Surveys (DHS) conduct surveys all over the world. ",
    "In these, they ask thousands of households about basic demographic variables, ",
    "but also about their health and wealth. ",
    "To detect changes, it's important to find countries where these surveys were ",
    "<span style='color:{greens[3]}'>repeated."
  )
cap <- paste0("**Data**: DHS program, {rnaturalearth} <br> ", social)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(data = grat,
          fill = "#d7ecfa",
          col = "#d7ecfa") +
  geom_sf(data = world_time_series, fill = "white") +
  geom_sf(data = world_time_series,
          aes(fill = n_cross_section)) +
  scale_fill_manual(
    values = c("grey90", greens),
    na.translate = FALSE
  ) +
  coord_sf(crs = "ESRI:54009") +
  labs(title = title,
       subtitle = st,
       caption = cap,
       fill = "Number of cross sections") +
  guides(fill = guide_legend(title.position = "bottom", 
                             label.position = "top")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 30),
        legend.key.width = unit(50, "pt"),
        legend.title = element_text(size = 30, margin = margin(r = 30)),
        legend.margin = margin(t = 30, l = 50),
        legend.justification = "left",
        plot.title = element_textbox_simple(family = "roboto",
                                            size = 60,
                                            face = "bold",
                                            margin = margin(l = 50, r = 50)),
        plot.caption = element_textbox_simple(lineheight = 2,
                                              size = 30,
                                              family = "sans",
                                              margin = margin(t = 150, l = 50, b = 30, r = 50)),
        plot.subtitle = element_textbox_simple(size = 30,
                                               lineheight = 1.7,
                                               family = "sans",
                                               vjust = 1.5,
                                               margin = margin(t = 60, b = 150, l = 50, r = 50)),
        )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-01-02", paste0("20240102", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
