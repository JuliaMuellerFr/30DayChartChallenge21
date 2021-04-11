# Day 3: Historical

library(extrafont)
library(ggtext)
library(tidyverse)

space <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/faca0b6bd282998693007c329e3f4b917a5fd7a8/data/2019/2019-01-15/launches.csv")

space %>% 
  filter(category == "O") %>% 
  group_by(launch_year, agency_type) %>% 
  count() %>% 
  ggplot + 
  aes(x = launch_year, y = n, fill = agency_type) +
  geom_area(size = 1.5, colour = "white") +
  scale_fill_manual(values = c("#784284", "#4D3A81", "#2D3585")) +
  scale_y_continuous(breaks = seq(0, 150, 10)) +
  scale_x_continuous(breaks = seq(1960, 2015, 5)) +
  labs(fill = "Type of agency",
       x = NULL, 
       y = NULL,
       title = "The space race",
       subtitle = "...is increasingly fought out among non-state agencies.",
       caption = "<br>Number of successful missions 1957 - 2018.<br><br>**Data source:** The Economist via Tidy Tuesday") +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        text = element_text(size = 14, family = "Bahnschrift", color = "white"),
        plot.title = element_markdown(hjust = 0.5, size = 22),
        plot.subtitle = element_markdown(hjust = 0.5, size = 16),
        plot.caption = element_markdown(hjust = 0.5, size = 14),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        legend.background = element_rect(fill = "transparent"))
