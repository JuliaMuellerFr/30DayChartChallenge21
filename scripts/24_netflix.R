# Day 24: Monochrome

library(tidyverse)
library(ggforce)
library(extrafont)

netflix <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

netflix_g <- netflix %>% 
  filter(type == "Movie") %>% 
  mutate(duration = parse_number(duration)) %>% 
  filter(str_detect(country, "Germany") == TRUE) 

netflix_g %>% 
  ggplot() +
  aes(x = release_year, y = duration, alpha = duration) +
  geom_point(color = "#FF0000", size = 6) +
  labs(title = "Films on Netflix with German involvement",
       x = "Release year", y = "Duration",
       caption = "\nTitles added for films that I've seen. \nData source: Kaggle (credit: Shivam Bansal) collected with Flixable") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1970, 2020, 5)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(20, 200, 20),
                     label = function(x) {return(paste(x, "minutes"))}) +
  coord_cartesian(ylim = c(10, 210), xlim = c(1968, 2022)) +
  theme(axis.line = element_line(color = "black"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 14),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        text = element_text(size = 12, family = "Segoe UI Light", color = "black"),
        plot.title = element_text(hjust = 0.5, size = 24),
        plot.caption = element_text(hjust = 0.5, size = 12),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        legend.position = "none") + 
  geom_mark_circle(aes(fill = title, label = title, 
                       filter = title %in% c("V for Vendetta", "Cloud Atlas", "Making Unorthodox", "About a Boy", "The Lives of Others")),
                   expand = unit(4, "mm"),
                   alpha = 0,
                   label.lineheight = 1,
                   label.fontsize = c(10, 8),
                   label.family = "Segoe UI Light")
