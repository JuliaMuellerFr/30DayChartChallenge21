# Day 1: part-to-whole

library(gggibbous)
library(extrafont)
library(ggtext)
library(tidyverse)

movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

movies <- movies %>% 
  separate(genre, sep = ", ", into = c("genre_clean", "g2", "g3", "g4")) %>% 
  drop_na(genre_clean) %>% 
  mutate(genre_clean = as_factor(genre_clean)) %>% 
  group_by(genre_clean, binary) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(genre_clean, desc(binary)) %>% 
  group_by(genre_clean) %>% 
  mutate(total = sum(n)) %>%
  ungroup() %>% 
  filter(total > 6) %>% 
  mutate(ratio = n / (total),
         right = if_else(binary == "PASS", TRUE, FALSE),
         pass_ratio = case_when(binary == "PASS" ~ ratio)) %>% 
  fill(pass_ratio) %>% 
  mutate(genre_clean = fct_reorder(genre_clean, pass_ratio)) 

movies %>% 
  ggplot +
  aes(x = genre_clean, y = 1, ratio = ratio, right = right, fill = right, size = total) +
  geom_moon()  +
  coord_fixed(ylim = c(0.5, 1.5)) +
  scale_size(range = c(20, 50)) +
  labs(x = NULL, y = NULL, 
       title = 'Bechdel test <b style = "color:#ef9d10;">pass</b>/<b style = "color:#514538;">fail</b> rates by film genre',
       subtitle = "A film passes the Bechdel test if  <br>(1) it has at least **two named women** (2) who **talk to each other** (3) about **anything but a man**. <br> Shouldn't be that hard... *right?*",
       caption = "<br>Includes 1572 films from 1970 - 2020. Larger circles indicate more data points.<br>**Data source:** Bechdeltest.com API via Tidy Tuesday") +
  scale_fill_manual(values = c("#514538", "#ef9d10")) +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        text = element_text(size = 12, family = "Dubai Light"),
        plot.title = element_markdown(hjust = 0.5, size = 18),
        plot.subtitle = element_markdown(hjust = 0.5, size = 12),
        plot.caption = element_markdown(hjust = 0.5, size = 10))

