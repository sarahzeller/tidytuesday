library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)
library(ggrepel)
library(patchwork)

#add fonts
font_add_google("Lilita One")
font_add(family = "fa",
         regular = "fontawesome/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Open Sans")
showtext_auto()

# set camcorder settings
showtext::showtext_opts(dpi = 300)

# let's go hacky for the caption
tab <- function(tab_width){
  paste0(
    "<span style='color:transparent;'>",
    rep(".", tab_width) |> paste(collapse = ""),
    "</span>"
  )
}

caption_text <- paste0(
  "<span style='font-family:fa;'>&#xf09b;</span>",
  tab(2),
  "sarahzeller",
  tab(4),
  "<span style='font-family:fa;'>&#xf099;</span>",
  tab(2),
  "@sarah_y_zeller",
  tab(2),
  "Data: Grants.gov | #tidytuesday"
)

subtitle_text <- paste("Now that the R4DS Online Learning Community is a public charity,",
                       "it's interesting to see which grants they could apply for.",
                       "I listed the grants that are still open and checked for the",
                       "estimated funding and the time that is still left to apply.",
                       "Seems like there are loads of projects that could be interesting!")

# get the data
tuesdata <- tidytuesdayR::tt_load('2023-10-03')
grants <- tuesdata$grants
grant_opportunity_details <- tuesdata$grant_opportunity_details

# start recording
gg_record(
  dir = file.path("2023", "2023-10-03", "recording"),
  # where to save the recording
  device = "png",
  # device to use to save images
  width = 25,
  # width of saved image
  height = 25,
  # height of saved image
  units = "cm",
  # units for width and height
  dpi = 300 # dpi to use when saving image
)

filtered_grants <- grants |>
  mutate(expected_awards = as.integer(expected_number_of_awards)) |>
  filter(estimated_funding > 0 & expected_awards > 0) |>
  filter(opportunity_status %in% c("Forecasted", "Posted")) |>
  mutate(estimated_funding_per_project = estimated_funding / expected_awards) |>
  mutate(days_left = as.integer(close_date - today())) |>
  mutate(
    number_awards_class = case_match(
      expected_awards,
      0:quantile(expected_awards, .25) ~ "\U2264 3",
      (quantile(expected_awards, .25) + 1):quantile(expected_awards, .5) ~ "\U2264 10",
      (quantile(expected_awards, .5) + 1):quantile(expected_awards, .75) ~ "\U2264 25",
      (quantile(expected_awards, .75) + 1):max(expected_awards) ~ "\U003E 25"
    ) |> factor(levels = c("\U2264 3", "\U2264 10", "\U2264 25", "\U003E 25"),
                ordered = TRUE)
  ) |> 
  filter(days_left > 0)

special_grants <- filtered_grants |> 
  filter(estimated_funding_per_project %in% c(max(estimated_funding_per_project), min(estimated_funding_per_project)) |
           expected_awards == max(expected_awards)) 

scatter_plot <- 
  filtered_grants |>
  ggplot() +
  geom_jitter(aes(x = days_left,
                  y = estimated_funding_per_project,
                  col = number_awards_class)) +
  scale_colour_viridis_d(begin = 1, end = 0) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(x = "Days left to apply",
       y = "Estimated funding per project",
       col = "Estimated number \nof awards") +
  geom_label_repel(data = special_grants,
                aes(label = paste(opportunity_title, "by", agency_code), 
                    x = days_left,
                    y = estimated_funding_per_project),
               colour = "grey60",
               nudge_x = .3, 
               nudge_y = -.3,
               family = "Open Sans") +
  sarahsFunctions::clean_theme() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

scatter_plot +
  plot_annotation(subtitle = subtitle_text,
                  title = "Time to check out grants!",
                  caption = caption_text) &
  theme(text = element_text(family = "Open Sans"),
        plot.title = element_textbox_simple(family = "Lilita One",
                                            size = 25,
                                            lineheight = .5,
                                            margin = margin(b = .5, t = 1, unit = "cm")),
        plot.subtitle = element_textbox_simple(
          colour = "black",
          linewidth = 0.6,
          lineheight = 1.2,
          margin = margin(b = 1.5, t = 1, unit = "cm"),
          size = 20
        ), 
        plot.caption = element_textbox_simple(
          lineheight = 1.5,
          margin = margin(t = 1, b = .5, unit = "cm"),
          size = 15),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.key.height = unit(.5, "cm"),
        legend.key.width = unit(.5, "cm"))



# save gif
gg_playback(
  name = file.path("2023", "2023-10-03", "20231003.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
