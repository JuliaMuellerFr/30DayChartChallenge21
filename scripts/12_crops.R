#Day 12

library(extrafont)
library(tidyverse)
library(ggstream)
library(ggtext)

key_crop_yields <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv',
                            col_names = c("country", "code", "year", "wheat", "rice", "maize", "soybeans", "potatoes", "beans", "peas", "cassava", "barley", "cocoa", "bananas"),
                            skip = 1)

key_crop_yields %>% 
  select(-c(country, code)) %>% 
  pivot_longer(cols = -year, names_to = "crop", values_to = "tonnes") %>% 
  drop_na(tonnes) %>%
  group_by(year, crop) %>% 
  summarise(yield = sum(tonnes)) %>% 
  ggplot() +
  aes(year, yield, fill = crop) +
  geom_stream() +
  geom_stream_label(aes(label = crop), 
                    family = "High Tower Text",
                    size = 8) +
  labs(
    title = "World-wide **crop yields** 1961 - 2018",
    x = NULL, y = NULL,
    caption = "<br>**Data source:** *Our World in Data* via Tidy Tuesday"
  ) +
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  scale_colour_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid")) +
  scale_fill_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid")) +
  theme(axis.line = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = "#290b06"),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        text = element_text(size = 18, family = "High Tower Text", color = "#290b06"),
        plot.title = element_markdown(hjust = 0.5, size = 30),
        plot.subtitle = element_markdown(hjust = 0.5, size = 18),
        plot.caption = element_markdown(hjust = 0.5, size = 16),
        plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"))
