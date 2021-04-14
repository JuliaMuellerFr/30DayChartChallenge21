# Day 14: Space

library(tidyverse)
library(ggshadow)
library(extrafont)
library(ggtext)
library(trekcolors)

ufo <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

ufo_sum <- ufo %>% 
  mutate(date_time_re = lubridate::mdy_hm(date_time)) %>% 
  mutate(year = lubridate::year(date_time_re)) %>% 
  filter(ufo_shape != "other" & ufo_shape != "unknown" & year >= 1990) %>% 
  group_by(ufo_shape) %>% 
  filter(n() > 4900) %>% 
  ungroup() %>% 
  group_by(year, ufo_shape) %>% 
  count() %>% 
  ungroup()

ufo_sum %>% 
  ggplot() +
  aes(x = year, y = n, colour = ufo_shape) +
  geom_glowline(size = 1.5)  +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1990, 2010, 5)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(200, 1600, 200)) +
  coord_cartesian(ylim = c(0, 1600), xlim = c(1990, 2015)) +
  labs(x = 'Year',
       y = "How often was this shape reported?",
       colour = NULL,
       title = 'UFO "sightings" by shape 1990 - 2014',
       caption = '<br>Shows the five most commonly mentioned shapes except "other" and "unknown".<br>**Data source:** *The National UFO Reporting Center* via Tidy Tuesday') +
  #scale_colour_manual(values = c("#1685f8", "#f52789", "#faeb2c", "#5d1278", "#e900ff")) +
  #scale_colour_manual(values = cols) +
  scale_color_trek("lcars_2369") +
  theme(axis.line = element_line(color = "white"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(colour = "white", size = 12),
        axis.title = element_text(colour = "white", size = 16),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        text = element_text(size = 12, family = "Rockwell", color = "white"),
        plot.title = element_markdown(hjust = 0.5, size = 28),
        plot.caption = element_markdown(hjust = 0.5, size = 14),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        legend.position = c(.5, .9),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  guides(color = guide_legend(direction = "horizontal",
                              override.aes = list(size = 1.5)))

