library(tidyverse)
library(camcorder)
library(rnaturalearth)
library(sf)
library(showtext)
library(ggtext)
library(scatterpie)
library(showtext)

font_add_google("Piedra")
font_add_google("Source Sans Pro")

options(scipen = 999)

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

founder_crops_sf <- founder_crops |> 
  st_as_sf(coords = c("longitude", "latitude")) |> 
  st_set_crs(4326)  

non_founder_crops_sf <- founder_crops_sf |> 
  summarize(n = sum(prop*n),
            .by = c("site_name", "is_founder_crop")) |> 
  pivot_wider(id_cols = "site_name",
              names_from = "is_founder_crop",
              names_prefix = "founder_",
              values_from = "n")

prop_by_site <- founder_crops |> 
  summarize(founder_crop_n = sum(n), .by = c("site_name", 
                                             "founder_crop",
                                             "latitude", "longitude")) |> 
  drop_na() |> 
  mutate(founder_crop = gsub(" ", "_", founder_crop)) |> 
  pivot_wider(id_cols = c("site_name", "latitude", "longitude"),
              names_from = "founder_crop",
              values_from = "founder_crop_n",
              values_fill = 0)

site_n <- founder_crops |> 
  summarize(density = sum(n)/10e5, 
            .by = c("site_name", "latitude", "longitude"))

wheat_time <- founder_crops_sf |> 
  filter(founder_crop == "Wheat") |> 
  summarize(n = sum(n),
            prop = mean(prop),
            .by = c("phase", "age_start", "age_end", "taxon", "taxon_detail"))
  
founder_crop_names <- prop_by_site |> select(-site_name, -latitude, -longitude) |> 
  names()

# cropped world
world <- ne_countries(scale = "medium", 
                      returnclass = "sf")  

gg_record(
  dir = file.path("2023", "2023-04-18", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 13, # width of saved image
  height = 12, # height of saved image
  units = "cm", # units for width and height
  dpi = 300 # dpi to use when saving image
)

caption_text <- 'Data Source: The "Neolithic Founder Crops" in Southwest Asia: Research Compendium'

ggplot() +
  geom_rect(data = data.frame(),
            aes(xmin = -25,
                xmax = 55,
                ymin = 25,
                ymax = 75),
            fill = alpha("lightblue", 3)) +
  geom_sf(data = world,
          col = "white",
          fill = alpha("lightgoldenrod", .8)) +
  geom_scatterpie(aes(x = longitude,
                      y = latitude),
                  data = prop_by_site,
                  cols = founder_crop_names,
                  legend_name = "Crop") +
  # geom_sf(data = site_n,
  #            aes(size = n),
  #         alpha = .3) +
  # stat_density_2d_filled(data = site_n,
  #                 aes(x = longitude,
  #                     y = latitude,
  #                     fill = density),
  #                 geom = "polygon",
  #                 contour_var = "density") +
  coord_sf(xlim = c(min(founder_crops$longitude - 3),
                    max(founder_crops$longitude + 3)), 
           ylim = c(min(founder_crops$latitude - 3),
                    max(founder_crops$latitude + 3)), 
           expand = FALSE) +
  theme_void() +
  theme(
        plot.caption = element_textbox_simple(vjust = 1,
                                              hjust = .2),
        plot.background = element_rect(fill = "#FCF5E5",
                                       colour = "#FCF5E5"),
        plot.title = element_text(family = "Piedra",
                             size = 70),
        text = element_text(family = "Source Sans Pro",
                            size = 30)) +
  labs(x = NULL,
       y = NULL,
       title = "Where were founder crops found?",
       col = "",
       caption = caption_text,
       size = "Number of seeds found \n(in 10,000s)")

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
