library(tidyverse)
library(janitor)
library(lubridate)
library(ggrepel)
library(geofacet)
library(ggh4x)
library(colorspace)

london_crime <- readr::read_csv("https://data.london.gov.uk/download/recorded_crime_summary/644a9e0d-75a3-4c3a-91ad-03d2a7cb8f8e/MPS%20LSOA%20Level%20Crime%20%28most%20recent%2024%20months%29.csv") %>% 
  clean_names()

london_crime_long <- london_crime %>% 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "date",
    names_prefix = "",
    values_to = "count",
    values_drop_na = TRUE
  ) %>% 
  mutate(
    date = ym(gsub("x", "", date)),
    month = month(date),
    year = year(date)
    )


# cross-london sum of crimes
london_crime_long_summarised <- london_crime_long %>% 
  select(-minor_category) %>% 
  group_by(major_category, year, month, date) %>% 
  summarise(
    count = sum(count)
  ) 
# %>% 
#   pivot_wider(
#     names_from = major_category,
#     values_from = count
#   )

ggplot(london_crime_long_summarised, aes(x = date, y = count, colour = major_category)) +
  geom_line() +
  scale_y_continuous(name = "Count") +
  scale_x_date(date_breaks = "3 months", date_labels = "%m/%Y") +
  ggtitle("Crimes in London") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#f6f4f2", color = NA),
    legend.position = "none",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey")
  ) +
  geom_label_repel(
    aes(label = major_category), 
    data = london_crime_long_summarised %>% filter(year %in% c("2021"), month %in% c("5")), 
    colour = "black", 
    size = 3
    ) 


# Movement of minor category within major categories

london_crime_long_summarised_sub <- london_crime_long %>% 
  group_by(major_category, minor_category, year, month, date) %>% 
  summarise(
    count = sum(count)
  ) 

ggplot(london_crime_long_summarised_sub %>% filter(major_category != "Miscellaneous Crimes Against Society" & major_category != "Possession of Weapons" & major_category != "Public Order Offences"), 
       aes(x = date, y = count, colour = minor_category)) +
  geom_line() +
  scale_y_continuous(name = "Count") +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
  ggtitle("Types of Crimes in London") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#f6f4f2", color = NA),
    legend.position = "none",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey")
  ) +
  facet_wrap(major_category ~ minor_category,
             shrink = TRUE) 
# +
#   geom_label_repel(
#     aes(label = minor_category),
#     data = london_crime_long_summarised %>% filter(year %in% c("2021"), month %in% c("5")),
#     colour = "black",
#     size = 3
#   )


# London boroughs graphics

borough_names <- gb_london_boroughs_grid %>% 
  select(borough_code = code_ons, borough = name)

crime_boroughs <- london_crime_long %>% 
  select(-minor_category) %>% 
  group_by(borough, major_category, year, month, date) %>% 
  summarise(
    count = sum(count)
  ) %>% 
  pivot_wider(names_from = major_category, values_from = count) %>% 
  left_join(borough_names) %>% 
  clean_names() %>% rename(name = borough)

ggplot(crime_boroughs, aes(x = date)) +
  geom_line(aes(y = theft, colour = "Theft")) +
  geom_line(aes(y = violence_against_the_person, colour = "Violence against people")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
  scale_color_manual(values = c("#c66570", "#192439")) +
  scale_fill_manual(values = c(lighten("#3D85F7"), lighten("#C32E5A"), "grey60"), labels = c("more theft", "more violence", "same")) +
  facet_geo(vars(name), grid = "gb_london_boroughs_grid") +
  ggtitle("Theft and Violence against people, London (2019 - 2021)") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.875, 0.975),
    plot.background = element_rect(fill = "#f6f4f2", color = NA),
    strip.text = element_text(face = "bold", color = "grey20"),
    plot.margin = margin(20, 30, 20, 30),
    # plot.title = element_text(margin = margin(0, 0, -100, 0), size = 26, family = "KyivType Sans", face = "bold", vjust = 0, color = "grey25"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey")
  )


