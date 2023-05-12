library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)
library(sf)
library(usmap)
library(patchwork)

#add fonts
font_add_google("Source Sans Pro", "source-sans")
font_add_google("Sigmar One")
font_add(family = "fa",
         regular = "fontawesome/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()

# set camcorder settings
showtext::showtext_opts(dpi = 300)

# get the data
tuesdata <- tidytuesdayR::tt_load('2023-05-09')
childcare_costs <- tuesdata$childcare_costs
counties <- tuesdata$counties

childcare_location <- childcare_costs |> 
  # pivot childcare cost info
  pivot_longer(cols = starts_with(c("mc", "mfc")),
               names_to = "care_type",
               values_to = "median_cost") |> 
  # extract care location and age
  mutate(location = ifelse(substr(care_type, 1, 2) == "mf",
                           "at home",
                           "in a center"),
         age = case_when(grepl("sa", care_type) ~ "school",
                         grepl("_", care_type) ~ gsub(".*_", "", care_type))) |> 
  mutate(age = factor(age, levels = c("infant",
                                      "toddler", 
                                      "preschool",
                                      "school"),
                      ordered = TRUE)) |> 
  # classify county size
  mutate(county_size = case_when(total_pop < 100*1000 ~ "< 100k",
                                 total_pop < 500*1000 ~ "100k\u2013500k",
                                 total_pop < 1000*1000 ~ "500k\u20131m",
                                 .default = "> 1m") |> 
           factor(levels = c("< 100k",
                             "100k\u2013500k",
                             "500k\u20131m",
                             "> 1m"),
                  ordered = TRUE)) 

home_cheaper <- childcare_costs |> 
  mutate(fips = county_fips_code,
         childcare_home = case_when(
           mc_infant < mfcc_infant ~ "in a center",
           mc_infant >= mfcc_infant ~ "at home     ",
           is.na(mc_infant) | is.na(mfcc_infant) ~ "not\nmeasured"))

gg_record(
  dir = file.path("2023", "2023-05-09", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 22, # width of saved image
  height = 36, # height of saved image
  units = "cm", # units for width and height
  dpi = 300 # dpi to use when saving image
)

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
  "<br></br>",
  "Data: National Database of Childcare Prices | #tidytuesday"
  )

subtitle_text <- paste('After childbirth, parents have to decide where their child',
                       'should be taken care of: ',
                       '<span style="color:wheat3">at home</span> ',
                       'or <span style="color:#1F51FF">in a center</span>.',
                       'If the child stays home, at least one parent cannot continue',
                       'working, which reduces their income.',
                       'The childcare costs probably impact this decision.\n\n',
                       'Taking care of a',
                       'child at home is cheaper in almost all US counties in 2018;',
                       'this effect is even stronger in larger cities.')
  
boxplots <- 
  ggplot() +
  geom_boxplot(data = childcare_location |> filter(age == "infant"),
             aes(x = county_size,
                 y = median_cost,
                 fill = location),
             key_glyph = "rect",
             varwidth = TRUE) +
  scale_fill_manual(values = c("at home" = "wheat",
                               "in a center" = "#1F51FF")) +
  sarahsFunctions::clean_theme() +
  theme(
    text = element_text(size = 15,
                        family = "source-sans"),
    axis.text.x = element_text(size = 15,
                               lineheight = .3,
                               hjust = .5),
    axis.text.y = element_text(size = 15,
                               lineheight = .3),
        plot.margin = margin(t = 2, l = 1, r = 1, unit = "cm"),
        legend.position = "top") +
  labs(fill = "2018 infant childcare costs",
       x = "County population",
       y = "Median weekly childcare cost (USD)",
       caption = "Note: Wider boxplots indicate more counties in this category.")

map <- 
  plot_usmap(regions = "counties",
           data = home_cheaper |> filter(study_year == 2018),
           values = "childcare_home",
           col = alpha("black", .1),
           lwd = .4) +
  scale_fill_manual(values = c("at home     " = alpha("wheat", .8),
                               "in a center" = "#1F51FF"),
                    na.value = "white") +
  labs(fill = "In 2018, childcare is cheaper  ",
       x = NULL,
       y = NULL) +
  theme(text = element_text(size = 60,
                            lineheight = .4,
                            family = "source-sans"),
        legend.position = "top",
        legend.background = element_rect(fill = NA,
                                         colour = NA))
  
map / boxplots +
  plot_layout(widths = 1,
              heights = 1
              ) +
  plot_annotation(subtitle = subtitle_text,
       title = "Childcare is cheaper at home",
       caption = caption_text) &
  theme(text = element_text(family = "source-sans",
                            size = 20),
        plot.title = element_textbox_simple(family = "Sigmar One",
                                            size = 25,
                                            lineheight = .5,
                                            margin = margin(b = .5, t = 1, unit = "cm")),
        plot.subtitle = element_textbox_simple(
          colour = "black",
          linewidth = 0.6,
          lineheight = 1,
          margin = margin(b = 1.5, t = 1, unit = "cm"),
          size = 20
        ), 
        plot.caption = element_textbox_simple(
          lineheight = 1.5,
          margin = margin(t = 1, b = .5, unit = "cm"),
          size = 15),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.key.height = unit(.5, "cm"),
        legend.key.width = unit(.5, "cm"),
        plot.background = element_rect(fill = "#bbdbfa",
                                       colour = "#bbdbfa"))

# save gif
gg_playback(
  name = file.path("2023", "2023-05-09", "20230509.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "#bbdbfa"
)
