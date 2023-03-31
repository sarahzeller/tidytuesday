#load libraries
library(tidyverse)
library(countrycode)

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
# TODO: filter for "real" places
transitions_per_country <- transitions_tidy |> 
  summarize(no_trans = n(),
            .by = zone)


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