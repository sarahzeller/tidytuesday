library(tidyverse)
library(camcorder)
library(rnaturalearth)
library(sf)
library(showtext)
library(ggtext)
library(patchwork)
library(emojifont)

#add fonts
font_add_google("Source Sans Pro")
font_add_google("Righteous")
font_add(family = "fa",
         regular = "fontawesome/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()

# get the data
tuesdata <- tidytuesdayR::tt_load('2023-04-25')

winners <- tuesdata$winners
london_marathon <- tuesdata$london_marathon

# load world coordinates
world <- ne_countries(scale = "medium", 
                      returnclass = "sf")  

# filter for Wheelchair winners
wheelchair_winners <- winners |> 
  filter(grepl("Wheelchair", Category)) |> 
  count(Category, Nationality) |> 
  merge(world |> 
          filter(grepl("country", type, ignore.case = TRUE)), 
        by.x = "Nationality",
        by.y = "admin",
        all.x = TRUE) |> 
  mutate(geometry = st_centroid(geometry),
         Category = gsub("Wheelchair", "", Category)) |> 
  select(Category, Nationality, n, geometry) |> 
  st_as_sf() 

gg_record(
  dir = file.path("2023", "2023-04-25", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 18, # width of saved image
  height = 12, # height of saved image
  units = "cm", # units for width and height
  dpi = 300 # dpi to use when saving image
)

gg_resize_film(height = 10, units = "cm")

caption_text <- paste0(#"<span style='font-family:Source Sans Pro'>",
                       "Sarah Zeller ",
                       # "</span>  ",
                       "<span style='font-family:fa;'>&#xf09b; </span>",
                       "szeller42",
                       # "<span style='font-family:Source Sans Pro'>",
                       "  | Data: {LondonMarathon} by Nicola Rennie | #tidytuesday")#,
                       # "</span>")
subtitle_text <- paste0('The London Marathon has taken place since ',
                        winners |> filter(grepl("Wheelchair", Category)) |> 
                          pull(Year) |> min(),
                        ". Wheelchair users are rated in an own category. ",
                        "Winners -- both for men and women -- appear to be ",
                        "mostly from Europe, and most are from the United Kingdom.")
  
  
plot <- 
  ggplot() +
  geom_sf(data = world,
          col = NA,
          fill = "black",
          alpha = .2) +
  geom_sf(data = wheelchair_winners,
          aes(size = n),
          col = alpha("red4", .6)) +
  facet_wrap(~Category) +
  coord_sf(crs = 8857,
           label_graticule = "SW") +
  theme_void() +
  theme(text = element_text(family = "Source Sans Pro",
                            size = 30,
                            colour = "white"),
        plot.title = element_textbox_simple(family = "Righteous",
                                  size = 60,
                                  margin = margin(l = 1.5, b = .5, t = .3, unit = "cm")),
        plot.subtitle = element_textbox_simple(
          linewidth = 0.6,
          lineheight = 0.4,
          margin = margin(b = .5, l = 1.5, r = 2, unit = "cm")
        ), 
        plot.caption = element_textbox_simple(
          vjust = 1,
          hjust = .6,
          margin = margin(b = .2, t = .3, l = 1.5, unit = "cm"),
          size = 20),
        strip.text = element_textbox_simple(colour = "black",
                                            margin = margin(t = .2, unit = "cm"),
                                            family = "Righteous"),
        plot.background = element_rect(fill = alpha("red4", .4),
                                       colour = alpha("red4", .4)),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        strip.background = element_rect(fill = "white",
                                        color = "white"),
        plot.margin = margin(b = -1.2, unit = "cm"),
        legend.position = "top",
        legend.key.width = unit(.1, "cm"),
        legend.key.height = unit(.2, "cm"),
        legend.text = element_text(size = 25),
        strip.text.x = 
          element_textbox_simple(size = 40,
                                 margin = margin(t = .5, b = .5, l = 4, unit = "cm")),
        panel.spacing = unit(-.5, "cm")
        ) +
  labs(subtitle = subtitle_text,
       title = "London marathon wheelchair winners",
       size = "Number")

black_bar <- ggplot() + 
  annotate("rect",
           xmin = 0, xmax = 40, ymin = 0, ymax = 1,
           fill = "black") +
  geom_richtext(aes(x = 18, y = .5,
                    label = caption_text),
                fill = NA,
                label.color = NA,
                col = "white",
                size = 10) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black",
                                       colour = "black"),
        text = element_text(family = "Source Sans Pro"))

# pin hole
circle <- ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), 
                       fill = "white", col = NA) +
  coord_fixed() +
  theme_void()

# arrange as bib
plot_w_annotation <- plot / black_bar +
  plot_layout(heights = c(10, 4)) +
  # bottom left hole
  inset_element(circle, -.75, 0.2, 0.8, 0.4,
                align_to = "full", on_top = TRUE) +
  # bottom right hole
  inset_element(circle, 0.95, 0.4, 1, 0.2,
                align_to = "full", on_top = TRUE) +
  # top left hole
  inset_element(circle, -.75, 6.1, 0.8, 6.3,
                align_to = "full", on_top = TRUE) +
  # top right hole
  inset_element(circle, .95, 6.1, 1, 6.3,
                align_to = "full", on_top = TRUE) 

ggsave(plot_w_annotation,
       filename = "2023/2023-04-25/20230425.png",
       width = 16, # width of saved image
       height = 12, # height of saved image
       units = "cm", # units for width and height
       dpi = 300)

# save gif
gg_playback(
  name = file.path("2023", "2023-04-25", "20230425.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
