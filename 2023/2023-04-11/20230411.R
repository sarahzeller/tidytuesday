# load libraries
library(tidyverse)
library(camcorder)
library(data.table)
library(dtplyr)
library(ggtext)
library(showtext)
library(sarahsFunctions)

font_add_google("Crete Round")
font_add_google("Roboto")

# read in data
tuesdata <- tidytuesdayR::tt_load('2023-04-11')

eggproduction <- tuesdata$`egg-production`
cagefreepercentages <- tuesdata$`cage-free-percentages`

hens_per_prod <- eggproduction |> 
  as.data.table() |> 
  select(-n_eggs, -source) |> 
  dcast(observed_month + prod_process ~ prod_type , 
        value.var = "n_hens") |> 
  as.data.frame()

cagefreepercentages_long <- cagefreepercentages |> 
  as.data.table() |> 
  melt(id.vars = c("observed_month", "source"),
       value.name = "percentage",
       variable.name = "product") |> 
  mutate(product = gsub("percent_", "", x = product))

eggproduction_perc <- eggproduction |> 
  as.data.table() |> 
  filter(prod_type == "table eggs") |> 
  select(-source, -prod_type) |> 
  # reshape
  melt(id.vars = c("observed_month", "prod_process"),
       variable.name = "measuring") |> 
  mutate(prod_process = case_when(
    prod_process == "all" ~ "all",
    prod_process == "cage-free (non-organic)" ~ "cage_free_non_organic",
    prod_process == "cage-free (organic)" ~ "cage_free_organic"
  )) |> 
  dcast(observed_month + measuring ~ prod_process,
        value.var = "value") |> 
  # calculate percentages
  mutate(perc_non_organic = cage_free_non_organic/all,
         perc_organic = cage_free_organic/all,
         perc_overall = (cage_free_non_organic + cage_free_organic) / all) |> 
  melt(id.vars = 1:3,
       measure.vars = patterns("perc_"),
       variable.name = "cage_free_type",
       value.name = "percentage") |> 
  mutate(cage_free_type = gsub("perc_", "", cage_free_type) |> 
           gsub("_", " ", x = _))


gg_record(
  dir = file.path("2023", "2023-04-11", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 5.1, # width of saved image
  height = 4.7, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

cols <- c("organic" = "#002868", "non organic" = "#BF0A30")

ggplot() +
  geom_area(data = eggproduction_perc |> 
              filter(measuring == "n_eggs" & cage_free_type != "overall"),
            aes(x = observed_month,
                y = percentage * 100,
                fill = cage_free_type,
                col = cage_free_type)) +
  labs(title = "US cagefree table-egg production",
       subtitle = "In the USA, the share of cage-free in overall table egg production increases, but is driven by non-organic production.",
       caption = "Source: OSF's US Egg Production Data Set",
       x = "",
       y = "Share of table egg production (%)",
       fill = "") +
  guides(fill = guide_legend(label.position = "top"),
         color = "none") +
  expand_limits(y = 0) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = alpha(cols, .4)) +
  scale_color_manual(values = cols) +
  clean_theme() +
  theme(text = element_text(size = 30,
                            family = "Roboto",
                            colour = "grey30"),
        axis.title = element_text(size = 35,
                                  colour = "grey20"),
        axis.text.x = element_text(size = 30,
                                   hjust = 1),
        axis.text.y = element_text(size = 30),
        plot.title = element_text(size = 60, 
                                  margin = margin(b = .2, unit = "in"),
                                  family = "Crete Round"),
        plot.title.position = "plot",
        plot.subtitle = element_textbox_simple(size = 35,
                                               linewidth = 0.5,
                                               lineheight = 0.4,
                                               margin = margin(b = .1, unit = "in")),
        legend.position = c(0.2, 0.9),
        legend.spacing.y = unit(0.01, "in"),
        legend.spacing.x = unit(0.3, "in"),
        legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0.9),
        axis.line.x = element_blank())

# save gif
gg_playback(
  name = file.path("2023", "2023-04-11", "20230411.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25
)
