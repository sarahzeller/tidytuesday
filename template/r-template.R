
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(date_chr)


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""


# Data wrangling ----------------------------------------------------------



# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path(yr, date_chr, "recording"),
  device = "png",
  width = 7,
  height = 5,
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
title <- ""
st <- ""
cap <- paste0(
  "**Data**: <br>", social
)


# Plot --------------------------------------------------------------------



# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path(yr, date_chr, paste0(date_strip, ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
