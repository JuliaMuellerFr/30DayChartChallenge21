# Day 22: Animation

library(tidyverse)
library(gganimate)
library(extrafont)
library(ggtext)

options(scipen = "999")

brazil_loss <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')

brazil <- brazil_loss %>% 
  select(-c(entity, code)) %>%
  pivot_longer(cols = -year, names_to = "reason", values_to = "amount") %>% 
  mutate(year = as.integer(year),
         reason = fct_reorder(reason, amount),
         reason = fct_recode(reason,
                             "Commercial crops" = "commercial_crops",
                             "Flooding due to dams" = "flooding_due_to_dams",
                             "Natural disturbances" = "natural_disturbances",
                             "Pasture for livestock" = "pasture",
                             "Logging for lumber" = "selective_logging",
                             "Fire loss" = "fire",
                             "Mining" = "mining",
                             "Infrastructure" = "other_infrastructure",
                             "Roads" = "roads",
                             "Tree plantations" = "tree_plantations_including_palm",
                             "Small-scale clearing" = "small_scale_clearing")) 

brazil_animated <- brazil %>% 
  ggplot +
  aes(x = reason, y = amount, fill = reason) +
  geom_bar(stat = "identity") +
  coord_cartesian(clip = "off") +
  coord_flip() +
  dutchmasters::scale_fill_dutchmasters(palette = "view_of_Delft") +
  theme(axis.line = element_blank(),
               axis.ticks = element_blank(),
               axis.text.y = element_text(colour = "black"),
               axis.text.x = element_blank(),
               panel.grid = element_blank(),
               panel.background = element_rect(fill = "white", color = "white"),
               plot.background = element_rect(fill = "white", color = "white"),
               text = element_text(size = 24, family = "Footlight MT Light", color = "black"),
               plot.title = element_markdown(hjust = 0.5, size = 30),
               plot.caption = element_markdown(hjust = 0.5),
               legend.position = "none",
        plot.margin = margin(t = 1, r = 5, b = 1, l = 1, unit = "cm")) +
  labs(title = 'Sources of **deforestation in Brazil** in {frame_time}',
       x = NULL, y = NULL,
       caption = '<br>**Data source**: *Our World in Data* via Tidy Tuesday') +
  transition_time(year) +
  ease_aes('linear')

animate(brazil_animated, height = 800, width = 800)
anim_save("C:/Users/Lenovo/Documents/GitHub/30DayChartChallenge21/plots/22_deforestation.gif")
