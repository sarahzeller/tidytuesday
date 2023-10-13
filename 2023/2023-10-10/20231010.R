
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(sf)
library(maps)
library(ggfx)

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-10")
haunted_places <- tuesdata$haunted_places

# Load fonts --------------------------------------------------------------

font_add_google("Clicker Script", "clicker_script")
font_add_google("Source Sans 3", "source_sans")
font_add(family = "fa",
         regular = "fontawesome/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- "black"
text_col <- "white"
highlight_col <- "#54A737"


# Data wrangling ----------------------------------------------------------

witches <- haunted_places |> 
  # search for witches
  filter(str_detect(description, "witch|wizard")) |>
  rowwise() |> 
  mutate(lat = ifelse(is.na(latitude), city_latitude, latitude),
         lon = ifelse(is.na(longitude), city_longitude, longitude)) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-10-10", "recording"),
  device = "png",
  width = 6,
  height = 9,
  units = "in",
  dpi = 300
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
title <- "Where witches haunt humans"
st <- paste("Witches haunt people all over the United States, and seem",
            "to take special interest in the Northeastern and Southwestern area.",
            "But is it possible",
            "that they are scared of the Midwest?")
cap <- paste0(
  "**Data**: The Shadowlands Haunted Places Index ", "<br>", social
)


# Plot --------------------------------------------------------------------

plot <- ggplot() + 
  geom_sf(data = witches, col = highlight_col) |> 
    with_outer_glow(colour = highlight_col, sigma = 20, expand = 10) +
  geom_polygon(data = map_data("state"),
               aes(x = long, y = lat, group = group), 
               fill = NA, col = text_col) +
  labs(title = title,
       subtitle = st,
       caption = cap) +
  theme_void(base_family = "source_sans") 


plot +
  theme(panel.margin = margin(50, 20, 50, 20),
        plot.margin = margin(20, 20, 20, 20),
        panel.background = element_rect(fill = bg_col),
        plot.background = element_rect(fill = bg_col),
        plot.title = element_textbox_simple(family = "clicker_script",
                                     face = "bold",
                                     colour = highlight_col,
                                     size = 150,
                                     lineheight = .2,
                                     halign = .5,
                                     margin = margin(l = 20, r = 20)),
        plot.caption = element_textbox_simple(colour = text_col,
                                              lineheight = .5,
                                              size = 50,
                                              margin = margin(t = 50, l = 20)),
        plot.subtitle = element_textbox_simple(size = 50,
                                        colour = text_col,
                                        lineheight = .5,
                                        hjust = .5,
                                        margin = margin(20, 20, 50, 20))
        ) 
  

# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-10-10", paste0("20231010", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
