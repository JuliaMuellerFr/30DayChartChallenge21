# Day 4: Magical

library(extrafont)
library(ggtext)
library(magick)
library(cowplot)
library(patchwork)
library(tidyverse)


theme_update(axis.ticks.y = element_blank(),
             axis.ticks.x = element_blank(),
             axis.text.x = element_text(colour = "white"),
             axis.text.y = element_text(colour = "white"),
             panel.grid = element_blank(),
             panel.background = element_rect(fill = "black", color = "black"),
             plot.background = element_rect(fill = "black", color = "black"),
             text = element_text(size = 18, family = "Blackadder ITC", color = "white"),
             plot.title = element_markdown(hjust = 0.5, size = 22),
             plot.subtitle = element_markdown(hjust = 0.5, size = 16),
             plot.caption = element_markdown(hjust = 0.5, size = 14),
             legend.position = "none")

H3_box <- image_read("https://upload.wikimedia.org/wikipedia/en/9/9b/Homm3boxart.jpg")

H3_cols <- c("#d54d3d", "#e95c17", "#ffe009", "#a97a0e", "#919191", "#6D8FB2", "#7295a9", "#4F6B81", "#454492")

H3 <- read_csv("C:/Users/Lenovo/Desktop/H3Units.csv") %>% 
  rename("location" = "Castle") %>% 
  filter(location != "Neutral") %>% 
  mutate(location = as_factor(location))


health_avg <-
  H3 %>%
  summarize(avg = mean(Health, na.rm = T)) %>%
  pull(avg)

health <- H3 %>% 
  mutate(location = fct_reorder(location, Health, .fun = "mean")) %>%
  ggplot + 
  aes(location, Health, fill = location, colour = location) +
  coord_flip() +
  geom_point(size = 1.5, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  geom_hline(aes(yintercept = health_avg), color = "gray70", size = 0.6) +
  scale_color_manual(values = H3_cols) +
  labs(x = NULL)


attack_avg <-
  H3 %>%
  summarize(avg = mean(Attack, na.rm = T)) %>%
  pull(avg)

att <- H3 %>% 
  mutate(location = fct_reorder(location, Attack, .fun = "mean")) %>%
  ggplot + 
  aes(location, Attack, fill = location, colour = location) +
  coord_flip() +
  geom_point(size = 1.5, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  geom_hline(aes(yintercept = attack_avg), color = "gray70", size = 0.6) +
  scale_color_manual(values = H3_cols) +
  labs(x = NULL)


defence_avg <-
  H3 %>%
  summarize(avg = mean(Defence, na.rm = T)) %>%
  pull(avg)

def <- H3 %>% 
  mutate(location = fct_reorder(location, Defence, .fun = "mean")) %>%
  ggplot + 
  aes(location, Defence, fill = location, colour = location) +
  coord_flip() +
  geom_point(size = 1.5, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  geom_hline(aes(yintercept = defence_avg), color = "gray70", size = 0.6) +
  scale_color_manual(values = H3_cols) +
  labs(x = NULL)


H3_title <- ggplot() +
  aes(x = c(0, 1), y = c(0, 1)) +
  geom_point() + 
  draw_image(H3_box, scale = 1.1) + 
  labs(title = "Which town to pick in **Heroes of Might and Magic 3**?",
       subtitle = "*Health*, *attack* and *defence* stats for each unit",
       caption = "**Data source:** Heroes of Might and Magic 3 Units <br>via Arthur Dayne on Kaggle") +
  labs(x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_markdown(hjust = 0.5, size = 22),
        plot.subtitle = element_markdown(hjust = 0.5, size = 16),
        plot.caption = element_markdown(hjust = 0.5, size = 14),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))


(H3_title + health)/(att + def)

