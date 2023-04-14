#load libraries
library(tidyverse)
library(countrycode)
library(rnaturalearth)
library(sarahsFunctions)

# read in data
tuesdata <- tidytuesdayR::tt_load('2023-03-28')

transitions <- tuesdata$transitions
timezones <- tuesdata$timezones
timezone_countries <- tuesdata$timezone_countries
countries <- tuesdata$countries

# add country names to tz info
tz_place_name <-
  timezone_countries |>
  mutate(
    place_name = country_code |>
      countrycode(origin = "iso2c",
                  destination = "country.name.en")) 

# check out number of timezones per country
tz_per_country <- tz_place_name |> 
  summarize(number_tz = n(),
            .by = c(country_code, place_name))

# tidy up transitions (NAs are - and + infinity)
transitions_tidy <- transitions |> 
  mutate(begin = lubridate::as_datetime(begin),
         end = lubridate::as_datetime(end)) 

# check out number of transitions per tz
transitions_per_country <- transitions_tidy |> 
  summarize(no_trans = n(),
            .by = zone) |> 
  merge(timezones, by = "zone") |> 
  sf::st_as_sf(coords = c("longitude", "latitude")) |> 
  sf::st_set_crs(4326)
  

# load world map
world <- ne_countries(scale = "medium", returnclass = "sf") 


# record
library(camcorder)
gg_record(
  dir = file.path("2023", "2023-03-28", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 5.1, # width of saved image
  height = 4.7, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

ggplot() +
  geom_sf(data = world,
          col = "white",
          fill = "lightgoldenrod") +
  geom_sf(data = transitions_per_country |> filter(no_trans > 0),
          aes(size = no_trans),
          alpha = .4,
          col = "cornflowerblue") +
  coord_sf(crs = 8857) +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(x = NULL,
       y = NULL,
       title = "How often did timezones change their time?",
       caption = "Source: IANA tz database, since 1834") +
  scale_size_binned_area(name = "Number of changes")

# save gif
gg_playback(
  name = file.path("2023", "2023-03-28", "20230328.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "#F0F5F5"
)
