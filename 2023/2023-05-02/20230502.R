library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)
library(patchwork)

#add fonts
font_add_google("Source Sans Pro", "source-sans")
font_add_google("Sigmar One")
font_add(family = "fa",
         regular = "fontawesome/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()

# get the data
tuesdata <- tidytuesdayR::tt_load('2023-05-02')

plots <- tuesdata$plots
species <- tuesdata$species

exclosure_plots <- plots |> 
  filter(treatment == "exclosure") |> 
  pull(plot)

granivores <- species |> 
  filter(granivore == 1) |> 
  pull(species)

surveys <- tuesdata$surveys |> 
  mutate(granivore = ifelse(species %in% granivores,
                            "Grain and seeds",
                            "Other"))
gg_record(
  dir = file.path("2023", "2023-05-02", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 18, # width of saved image
  height = 12, # height of saved image
  units = "cm", # units for width and height
  dpi = 300 # dpi to use when saving image
)


caption_text <- paste0("<span style='font-family: source-sans'>",
                       "Sarah Zeller ",
                       "</span>  ",
                       "<span style='font-family: fa;'>&#xf09b; </span>",
                       "szeller42",
                       "<span style='font-family: source-sans'>",
                       "  | Data: Portal project | #tidytuesday",
                       "</span>")
subtitle_text <- paste('The Portal project in Arizona, USA, has been studying rodents',
                        'in a field study since the 1970s. Once a month, rodents are',
                        'caught on several 50x50m plots. Their diet appears to',
                        'play a role in how much they weigh and how large their',
                        'feet are.')
  
  
ggplot() +
  geom_jitter(data = surveys,
             aes(x = hfl, 
                 y = wgt,
                 col = granivore),
             alpha = .4) +
  scale_colour_manual(values = c(alpha("#4f8b20", .3),
                                 "#633b0f")) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  sarahsFunctions::clean_theme() +
  theme(text = element_text(family = "source-sans",
                            size = 30),
        plot.title = element_textbox_simple(family = "Sigmar One",
                                  size = 60,
                                  margin = margin(b = .5, t = .3, unit = "cm")),
        plot.subtitle = element_textbox_simple(
          colour = "black",
          linewidth = 0.6,
          lineheight = 0.4,
          margin = margin(b = .5, r = 1, unit = "cm"),
          size = 35
        ), 
        plot.caption = element_textbox_simple(
          vjust = 1,
          hjust = .6,
          margin = margin(b = .2, unit = "cm"),
          size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.line.x = element_blank(),
        plot.background = element_rect(fill = "#F0DEB4",
                                       colour = "#F0DEB4"),
        panel.background = element_rect(fill = NA,
                                        color = "white",
                                        linewidth = 4),
        plot.margin = margin(l = 1, r = 1, unit = "cm"),
        legend.position = "bottom",
        legend.key.width = unit(.1, "cm"),
        legend.key.height = unit(.2, "cm"),
        legend.text = element_text(size = 25)) +
  labs(subtitle = subtitle_text,
       title = "Granivore rodents weigh less",
       col = "Diet",
       caption = caption_text,
       x = "Hindfoot length (cm)",
       y = "Weight (g)")


# save gif
gg_playback(
  name = file.path("2023", "2023-05-02", "20230502.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "#F0DEB4"
)
