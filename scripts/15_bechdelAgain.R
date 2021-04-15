# Day 15: Multivariate

options(scipen = 999)

library(extrafont)
library(ggtext)
library(ggforce)
library(tidyverse)

movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

mov <- movies %>% 
  mutate(title = str_replace(title, "&#39;", "'"),
         intgross = as.numeric(intgross),
         budget = budget/1000000) %>% 
  filter(clean_test != "dubious") %>% 
  drop_na(year, budget, intgross, clean_test)

p <- mov %>% 
  ggplot() +
  aes(x = year, y = budget) +
  geom_jitter(aes(size = intgross, colour = clean_test, fill = clean_test)) +
  scale_size_continuous(range = c(2, 10)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1970, 2010, 5)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(50, 450, 50)) +
  coord_cartesian(ylim = c(0, 450), xlim = c(1970, 2015)) +
  labs(title = "Back to the Bechdel test!",
       subtitle = 'Size shows the international gross in a film\'s release year. The colour indicates whether it:<br>
       <span style = "color:#800000FF;">doesn\'t have (named) female characters</span> | 
       <span style = "color:#FFB547FF;">the women don\'t talk to each other</span> | 
       <span style = "color:#e24f22;">or only talk about men</span> | 
       <span style = "color:#4A6990FF;">or talk about something else (= a pass!)</span><br>',
       x = "Year", y = "Budget (in million)",
       caption = "<br>Includes 1642 films from 1970 - 2013. <br>**Data source:** *Bechdeltest.com* API via Tidy Tuesday") +
  scale_color_manual(values = c("#e24f22", "#FFB547FF", "#800000FF", "#4A6990FF")) +
  theme(axis.line = element_line(color = "black"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text = element_text(colour = "black", size = 12),
      axis.title = element_text(colour = "black", size = 16),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.background = element_rect(fill = "white", color = "white"),
      text = element_text(size = 12, family = "Berlin Sans FB", color = "black"),
      plot.title = element_markdown(hjust = 0.5, size = 28, colour = "#152238"),
      plot.subtitle = element_markdown(hjust = 0.5, size = 16),
      plot.caption = element_markdown(hjust = 0.5, size = 14),
      plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
      legend.position = "none")

p + 
  geom_mark_circle(aes(fill = title, label = title, 
                       filter = title %in% c("Avatar", "Waterworld", "RoboCop", "Titanic"),
                       description = plot), 
                   expand = unit(7, "mm"),
                   alpha = 0,
                   label.lineheight = 0.7,
                   label.fontsize = c(12, 10),
                   label.family = "Berlin Sans FB")

