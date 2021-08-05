library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(janitor)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(maps)
library(proj4)
library(ggmap)
library(leaflet)

g <-
  read.csv(file = "Z:/Resources/Personal/Mihir Baxi/renewable-energy-planning-database-q1-march-2021.csv")


# Wind data ---------------------------------------------------------------


generators <-
  read.csv(file = "Z:/Resources/Personal/Mihir Baxi/renewable-energy-planning-database-q1-march-2021.csv") %>%
  clean_names() %>%
  filter(
    technology_type %in% c("Wind Onshore",
                           "Wind Offshore"),
    development_status %in% c("Operational")
  ) %>%
  select(
    ref_id,
    operator_or_applicant,
    site_name,
    technology_type,
    x_coordinate,
    y_coordinate,
    county,
    region,
    country,
    installed_capacity_mw = installed_capacity_m_welec
  )


gen = data.frame(x = generators$x_coordinate, y = generators$y_coordinate) %>%
  st_as_sf(coords = 1:2, crs = 27700)

gen_latlong = st_transform(gen$geometry, 4326)

gen_df <-
  as.data.frame(unlist(gen_latlong)) %>% rename(a = `unlist(gen_latlong)`) %>%
  mutate(ind = rep(c(1, 2), length.out = n())) %>%
  group_by(ind) %>%
  mutate(id = row_number()) %>%
  spread(ind, a) %>%
  select(-id) %>%
  rename(lat = `1`,
         long = `2`)

gen_coords <- cbind(generators, gen_df)





# Solar Data --------------------------------------------------------------



generators_solar <-
  read.csv(file = "Z:/Resources/Personal/Mihir Baxi/renewable-energy-planning-database-q1-march-2021.csv") %>%
  clean_names() %>%
  filter(
    technology_type %in% c("Solar Photovoltaics"),
    development_status %in% c("Operational")
  ) %>%
  select(
    ref_id,
    operator_or_applicant,
    site_name,
    technology_type,
    x_coordinate,
    y_coordinate,
    county,
    region,
    country,
    installed_capacity_mw = installed_capacity_m_welec
  ) %>% drop_na()

gen_solar = data.frame(x = generators_solar$x_coordinate, y = generators_solar$y_coordinate) %>%
  st_as_sf(coords = 1:2, crs = 27700)

gen_latlong_solar = st_transform(gen_solar$geometry, 4326)

gen_df_solar <-
  as.data.frame(unlist(gen_latlong_solar)) %>% rename(a = `unlist(gen_latlong_solar)`) %>%
  mutate(ind = rep(c(1, 2), length.out = n())) %>%
  group_by(ind) %>%
  mutate(id = row_number()) %>%
  spread(ind, a) %>%
  select(-id) %>%
  rename(lat = `1`,
         long = `2`)
gen_coords_solar <- cbind(generators_solar, gen_df_solar)


# Wind chart --------------------------------------------------------------
worldmap = map_data('world')



ggplot() +
  geom_polygon(
    data = worldmap,
    aes(x = long, y = lat,
        group = group),
    fill = 'springgreen4',
    color = 'springgreen4'
  ) +
  coord_fixed(ratio = 1.5,
              xlim = c(-9, 2),
              ylim = c(50, 60.5)) +
  geom_point(
    data = gen_coords %>%
      filter(technology_type %in% c("Wind Onshore",
                                    "Wind Offshore")),
    aes(
      x = lat,
      y = long,
      alpha = 0.9,
      color = 'firebrick4'
    )
  ) +
  geom_jitter() +
  theme(
    panel.background = element_rect(colour = 'steelblue1', fill = 'steelblue1'),
    panel.grid.major = element_line(colour = 'skyblue1'),
    panel.grid.minor = element_line(colour = 'skyblue1'),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none'
  )


# Solar chart -------------------------------------------------------------
worldmap = map_data('world')



ggplot() +
  geom_polygon(
    data = worldmap,
    aes(x = long, y = lat,
        group = group),
    fill = 'springgreen4',
    color = 'springgreen4'
  ) +
  coord_fixed(ratio = 1.5,
              xlim = c(-9, 2),
              ylim = c(50, 60.5)) +
  geom_point(data = gen_coords_solar,
             aes(
               x = lat,
               y = long,
               alpha = 0.9,
               color = 'gold1'
             ))  +
  geom_point(
    data = gen_coords %>%
      filter(technology_type %in% c("Wind Onshore",
                                    "Wind Offshore")),
    aes(
      x = lat,
      y = long,
      alpha = 0.9,
      color = 'firebrick4'
    )
  ) +
  theme_void() +
  geom_jitter() +
  theme(
    panel.background = element_rect(colour = 'steelblue1', fill = 'steelblue1'),
    panel.grid.major = element_line(colour = 'skyblue1'),
    panel.grid.minor = element_line(colour = 'skyblue1'),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none'
  )


# Combined chart ----------------------------------------------------------


ggplot() +
  geom_polygon(
    data = worldmap,
    aes(x = long, y = lat,
        group = group),
    fill = 'springgreen4',
    color = 'springgreen4'
  ) +
  coord_fixed(ratio = 1.5,
              xlim = c(-9, 2),
              ylim = c(50, 60.5)) +
  geom_point(data = gen_coords_solar,
             aes(
               x = lat,
               y = long,
               alpha = 0.9,
               color = 'yellow'
             )) +
  geom_jitter() +
  theme_void() +
  theme(
    panel.background = element_rect(colour = 'steelblue1', fill = 'steelblue1'),
    panel.grid.major = element_line(colour = 'skyblue1'),
    panel.grid.minor = element_line(colour = 'skyblue1'),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none'
  )

map <- leaflet() %>% addTiles()
