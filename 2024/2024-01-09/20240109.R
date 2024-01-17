


# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(grid)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-01-09")
canada_births <- tuesdata |> pluck("canada_births_1991_2022")
nhl_player_births <- tuesdata |> pluck("nhl_player_births")


# Load fonts --------------------------------------------------------------

font_add_google("DM Sans", "dm_sans")
font_add_google("Black Ops One", "black_ops")
font_add(family = "fa",
         regular = "fontawesome/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()


# Define colours ----------------------------------------------------------

text_col <- "#518ED2"
highlight_col <- "#cb0525"

# gradient function from
# https://stackoverflow.com/questions/30136725/plot-background-colour-in-gradient
make_gradient <- function(deg = 45,
                          n = 100,
                          cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(data = rep(seq(0, 1, length.out = n) * cos(rad), n),
                byrow = TRUE,
                ncol = n) +
    matrix(data = rep(seq(0, 1, length.out = n) * sin(rad), n),
           byrow = FALSE,
           ncol = n)
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"),
    interpolate = TRUE
  )
}

bg_col <- make_gradient(deg = 45,
                        n = 500,
                        cols = c("#afd5ff",
                                 "white",
                                 "#afd5ff",
                                 "white",
                                 "#afd5ff"))


# Data wrangling ----------------------------------------------------------
# for all of Canada
canada_yearly_births <- canada_births |>
  summarize(yearly_births = sum(births, na.rm = TRUE),
            .by = year)

canada_births_monthly_rates <- canada_births |>
  merge(canada_yearly_births, by = "year") |>
  mutate(birth_perc = births / yearly_births)

canada_birth_months <- canada_births |>
  summarize(total_births = sum(births, na.rm = TRUE),
            .by = month) |>
  mutate(birth_perc = total_births / sum(canada_births$births))

# for NHL players
nhl_birth_months <- nhl_player_births |>
  filter(birth_year >= 1991) |>
  summarize(total_births = n(), .by = c(birth_month))

nhl_birth_months <- nhl_birth_months |>
  mutate(birth_perc = total_births / sum(nhl_birth_months$total_births)) |>
  arrange(birth_month) |>
  rename(month = birth_month)

birth_months <- nhl_birth_months |>
  mutate(group = "NHL players") |>
  rbind(canada_birth_months |> mutate(group = "Canada"))

perc_labels <- data.frame(
  x = c(12, 12, 12),
  y = c(.08, .1, .12),
  label = c("8%", "10%", "12%")
)

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-01-09", "recording"),
  device = "png",
  width = 25,
  height = 25,
  units = "cm",
  dpi = 300
)


# Define text -------------------------------------------------------------

# let's go hacky for the caption
tab <- function(tab_width) {
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
title <- "NHL players are winter babies"
st <-
  paste0(
    "Canadians are born throughout the year, not favouring any month. ",
    "It's a different story for NHL players: They tend to be born earlier",
    " in the year. One explanation for this is that in early years, being slightly",
    " older makes a large difference in skills. Since players receive **crucial ",
    "training** during these years, being born early in the year is important even",
    " for older players."
  )
cap <- paste0("**Data** {tuesdata} (1991--2021) ", tab(4), social)


# Plot --------------------------------------------------------------------

# both
plot <- ggplot() +
  geom_col(data = birth_months,
           aes(x = month, y = birth_perc),
           fill = highlight_col) +
  facet_wrap( ~ group) +
  geom_text(
    data = perc_labels,
    aes(x, y, label = label),
    size = 15,
    family = "dm_sans",
    col = "grey60"
  ) +
  scale_x_continuous(
    expand = expansion(add = .05),
    breaks = c(1, 4, 7, 10),
    labels = month.abb[c(1, 4, 7, 10)]
  ) +
  scale_y_continuous(
    limits = c(0, 0.12),
    breaks = c(.05, .10, .15),
    minor_breaks = NULL,
    labels = NULL
  ) +
  labs(
    y = NULL,
    x = "",
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_polar() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 50, face = "bold"),
    axis.text.y  = element_text(size = 50),
    text = element_text(family = "dm_sans"),
    strip.text = element_textbox_simple(
      family = "black_ops",
      size = 70,
      margin = margin(l = 10, b = 10)
    ),
    plot.title = element_textbox_simple(
      family = "black_ops",
      size = 100,
      colour = text_col,
      margin = margin(l = 10, t = 10, b = 20)
    ),
    plot.subtitle = element_textbox_simple(
      family = "dm_sans",
      size = 50,
      colour = text_col,
      margin = margin(l = 10, t = 10, b = 50),
      lineheight = .5
    ),
    plot.caption = element_textbox_simple(
      family = "dm_sans",
      size = 50,
      colour = text_col,
      margin = margin(l = 10, b = 10, t = 20),
      lineheight = .5
    )
  )

background <- ggplot() +
  annotation_custom(
    grob = bg_col,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf
  ) + 
  theme_void() +
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_rect(fill = "transparent", colour = NA))

background +
  inset_element(
    plot,
    left = 0,
    bottom = 0,
    right = 1,
    top = 1,
    align_to = "full",
    clip = FALSE
  ) +
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_rect(fill = "transparent", colour = NA))

# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-01-09", paste0("20240109", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
