
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(urbnmapr)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-01-16")
polling_places <- tuesdata$polling_places


# Load fonts --------------------------------------------------------------

font_add_google("DM Sans", "dm_sans")
font_add(family = "fa",
         regular = "fontawesome/Font Awesome 6 Brands-Regular-400.otf")
font_add(family = "fa_solid",
         regular = "fontawesome/Font Awesome 6 Free-Solid-900.otf")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- ""
text_col <- "#8c7668"
highlight_col <- ""


# Data wrangling ----------------------------------------------------------

# get changes in polling places
timeseries_counties <- polling_places |> 
  drop_na(county_name) |> 
  # just get presidential elections
  filter(election_date %in% c("2012-11-06", "2020-11-03")) |> 
  mutate(election_year = lubridate::year(election_date)) |> 
  select(election_year, state, county_name) |> 
  # count number of polling places per year
  count(state, county_name, election_year) |> 
  pivot_wider(names_from = election_year,
              values_from = n,
              names_prefix = "election_") |> 
  # only include counties with dates for all elections
  drop_na(starts_with("election_")) |> 
  mutate(change_12_20 = (election_2020 - election_2012) / election_2012) |>
  # prepare for fuzzy matching
  mutate(county_name = str_replace(county_name, "&", "and")) |> 
  mutate(county_id = paste0(state, "_", tolower(county_name))) 

# get county geometries
states <- get_urbn_map(map = "states", sf = TRUE)
counties <- get_urbn_map(map = "counties", sf = TRUE) |> 
  mutate(county_name = str_replace(county_name, " (County|Parish)$", "")) |> 
  mutate(county_id = paste0(state_abbv, "_", tolower(county_name))) 
sf::st_crs(counties) <- 2163

# fuzzy match both
counties_matched <- counties |> 
  merge(timeseries_counties,
        by = "county_id",
        all.x = TRUE) |> 
  sf::st_as_sf(crs = 2163) |> 
  mutate(change_12_20_classes = 
           case_when(change_12_20 <= 1 & change_12_20 > .5 ~ 1,
                     change_12_20 <= .5 & change_12_20 > 0 ~ 2,
                     change_12_20 == 0 ~ 3,
                     change_12_20 < 0 & change_12_20 >= -.5 ~ 4,
                     change_12_20 < .5 ~ 5) |> 
           factor(labels = c("large increase",
                             "small increase",
                             "no change",
                             "small decrease",
                             "large decrease"),
                  ordered = TRUE))

cols <- RColorBrewer::brewer.pal(5, "BrBG")

draw_circle <- function(color) {
  "<span style='font-family:fa_solid;color:{col};'>&#xf111;</span>" |> 
    glue::glue(col = color)
}

colored_text <- function(color, text) {
  "**<span style='color:{col};'>{t}</span>**" |> 
    glue::glue(col = color, t = text) |> 
    paste0(draw_circle(color))
}


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-01-16", "recording"),
  device = "png",
  width = 25,
  height = 25,
  units = "cm",
  dpi = 300,
  bg = "#f7f3e4"
)


# Define text -------------------------------------------------------------

# let's go hacky for the caption
tab <- function(tab_width){
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
title <- "Less places to vote in 2020"
st <- glue::glue("Within US counties, I counted the polling places in ",
            " 2012 and 2020. ",
            "In most counties, the number of polling places ",
            "did not change {draw_circle(cols[3])}. ",
           "In some counties, this number ",
           "{colored_text(cols[2], 'increased by up to 50%')} or",
           " {colored_text(cols[1], 'even more')}, ",
           "but for others it {colored_text(cols[4], 'decreased by up to 50%')}",
           "{colored_text(cols[5], ' or more')}. ",
            "A lot of states are ",
            "not included in the dataset, some because citizens mostly use mail voting.")
cap <- paste0(
  "**Data**: The Center for Public Integrity <br> **Visualization**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  # base map
  geom_sf(data = states,
          fill = NA,
          col = "#e0cabc",
          lwd = .75) +
  geom_sf(data = counties_matched,
          fill = NA,
          col = "#e0cabc") +
  geom_sf(data = counties_matched |> drop_na(change_12_20_classes),
          aes(fill = change_12_20_classes),
          col = "#ad9180") +
  scale_fill_brewer(type = "div",
                    direction = -1) +
  labs(title = title,
       subtitle = st,
       caption = cap) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_textbox_simple(
          family = "dm_sans",
          face = "bold",
          size = 100,
          color = text_col,
          margin = margin(l = 20, t = 40, b = 20, r = 20)
        ),
        plot.subtitle = element_textbox_simple(
          family = "dm_sans",
          size = 50,
          color = text_col,
          margin = margin(l = 20, t = 10, b = 20, r = 20),
          lineheight = .5
        ),
        plot.caption = element_textbox_simple(
          family = "dm_sans",
          size = 50,
          color = text_col,
          margin = margin(l = 20, b = 20, t = 20, r = 20),
          lineheight = .5
        ))


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-01-16", paste0("20240116", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
