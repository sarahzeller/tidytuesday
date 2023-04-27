library(tidyverse)
library(camcorder)
library(rnaturalearth)
library(sf)
library(showtext)
library(ggtext)
library(showtext)
library(sfhotspot)

#add fonts
font_add_google("Piedra")
font_add_google("Source Sans Pro")
showtext_auto()

# get the data
tuesdata <- tidytuesdayR::tt_load('2023-04-18')
founder_crops <- tuesdata$founder_crops |> 
  # following https://www.r-bloggers.com/2023/04/tidytuesday-week-16-neolithic-founder-crops/
  mutate(founder_crop = recode(founder_crop,
                               "einkorn wheat" = "Wheat",
                               "emmer wheat" = "Wheat",
                               "chickpea" = "Legumes",
                               "bitter vetch" = "Legumes",
                               "lentil" = "Legumes",
                               "pea" = "Legumes",
                               "flax" = "Flax",
                               .default = stringr::str_to_sentence(founder_crop))) |> 
  mutate(is_founder_crop = !is.na(founder_crop))

# find largest non founder crop
largest_non_founder_crop <- founder_crops |> 
  summarize(n = sum(n),
            samples = n(),
            summed_prop = sum(prop),
            .by = c("is_founder_crop", "genus", "founder_crop")) |> 
  filter(is_founder_crop == FALSE & !is.na(genus)) |> 
  arrange(-summed_prop) |>   
  head(1) |> 
  pull(genus)

founder_crops_sf <- founder_crops |> 
  st_as_sf(coords = c("longitude", "latitude")) |> 
  # project it
  st_set_crs(4326)  

n_by_site <- founder_crops_sf |> 
  filter(genus == largest_non_founder_crop) |> 
  summarize(number_seeds = sum(n), .by = c("site_name", "geometry"))  

# calculate density
n_kde <- n_by_site |> 
  st_transform("ESRI:53035") |> 
  hotspot_kde(weights = number_seeds,
              grid_type = "hex",
              cell_size = 5000)

# cropped world
world <- ne_countries(scale = "medium", 
                      returnclass = "sf")  

# crop density to world
n_kde_cropped <- n_kde |> 
  st_intersection(world |> st_transform("ESRI:53035"))

gg_record(
  dir = file.path("2023", "2023-04-18", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 13, # width of saved image
  height = 12, # height of saved image
  units = "cm", # units for width and height
  dpi = 300 # dpi to use when saving image
)

caption_text <- 'Data Source: The "Neolithic Founder Crops" in Southwest Asia: Research Compendium'
subtitle_text <- 
  paste0("While it is often assumed that in Neolithic times, only the eight founder ",
        "crops were cultivated, researchers have found scores of other crops as well. ",
        "The most frequent one is ",
        largest_non_founder_crop,
        ".")


ggplot() +
  geom_rect(data = data.frame(),
            aes(xmin = -25,
                xmax = 55,
                ymin = 25,
                ymax = 75),
            fill = alpha("lightblue", 3)) +
  # just outlines
  geom_sf(data = world,
          col = NA,
          fill = alpha("lightgoldenrod", .8)) +
  # kde
  geom_sf(data = n_kde_cropped,
          aes(fill = kde/1000),
          # alpha = .5,
          colour = NA) +
  # sites
  geom_sf(data = n_by_site,
          alpha = .3,
          size = .1) +
  # borders
  geom_sf(data = world,
          col = "black",
          alpha = .8,
          fill = NA) +
  coord_sf(xlim = c(min(founder_crops$longitude - 3),
                    max(founder_crops$longitude + 3)), 
           ylim = c(min(founder_crops$latitude - 3),
                    max(founder_crops$latitude + 3)), 
           expand = FALSE) +
  annotate("text",
           x = 31, y = 33,
           label = "Mediterranean sea",
           family = "Source Sans Pro",
           colour = "grey30",
           size = 8,
           alpha = .6) +
  scale_fill_distiller(direction = 1,
                       labels = scales::comma) +
  theme_void() +
  theme(
        text = element_text(family = "Source Sans Pro",
                            size = 30),
        plot.title = element_text(family = "Piedra",
                                  size = 45),
        plot.subtitle = element_textbox_simple(
          linewidth = 0.6,
          lineheight = 0.1,
          margin = margin(b = .1, t = .1, unit = "in")
        ), 
        plot.caption = element_textbox_simple(
          vjust = 1,
          hjust = .2,
          margin = margin(b = .2, t = .1, unit = "in"),
          size = 20),
        plot.background = element_rect(fill = "#FCF5E5",
                                       colour = "#FCF5E5"),
        legend.position = "bottom",
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.2, "cm"),
        legend.text = element_text(size = 25)) +
  labs(title = paste0(largest_non_founder_crop,
                      ": the most common non-founder crop"),
       subtitle = subtitle_text,
       fill = paste(largest_non_founder_crop,
                    "seeds found (in thsd.)"),
       caption = caption_text)

# line chart: wheat over time
ggplot() +
  geom_area(data = wheat_time,
            aes(x = -age_start, y = prop, fill = taxon_detail )) +
  theme_classic() +
  labs(title = "Proportions of wheat types over time",
       col = "Wheat taxonomy",
       x = "",
       y = "Proportion of found seeds") +
  theme(
    plot.background = element_rect(fill = "#FCF5E5",
                                   colour = "#FCF5E5"),
    text = element_text(size = 30,
                            # family = "Roboto",
                            colour = "grey30"),
        axis.title = element_text(size = 35,
                                  colour = "grey20"),
        axis.text.x = element_text(size = 30,
                                   hjust = 1),
        axis.text.y = element_text(size = 30),
        plot.title = element_text(size = 60, 
                                  margin = margin(b = .2, unit = "in")),
        plot.title.position = "plot",
        plot.subtitle = element_textbox_simple(size = 35,
                                               linewidth = 0.5,
                                               lineheight = 0.4,
                                               margin = margin(b = .1, unit = "in")),
        legend.position = "bottom",
        legend.spacing.y = unit(0.01, "in"),
        legend.spacing.x = unit(0.3, "in"),
        legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0.9),
        axis.line.x = element_blank())


# save gif
gg_playback(
  name = file.path("2023", "2023-04-18", "20230418.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "#FCF5E5"
)
