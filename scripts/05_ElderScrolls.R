# Day 5: Slope

library(extrafont)
library(ggtext)
library(tidyverse)


games <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

ES <- games %>% 
  unite(col = m_y, c("month", "year"), sep = " ", remove = FALSE) %>% 
  mutate(m_y = lubridate::my(m_y)) %>% 
  filter(str_detect(gamename, "Elder Scrolls"),
         gamename != "The Elder Scrolls: Legends") %>% 
  group_by(gamename, year) %>% 
  summarise(avg_avg = mean(avg)) %>% 
  ungroup() %>% 
  filter(year > 2013)

ES %>% 
  ggplot() +
  aes(x = year, y = avg_avg, colour = gamename, fill = gamename) +
  geom_smooth(method = "lm", se = FALSE, colour = "#db9d11") +
  labs(title = "How many people play **Elder Scrolls** games?",
       subtitle = "Average number of players on **Steam** for *ESO*, *Oblivion*, and (my personal favourite) *Morrowind*.",
       caption = "**Data source**: Steam via Kaggle via Tidy Tuesday",
       x = "Year", 
       y = "Average number of players at the same time") +
  annotate(
    "text", x = 2020, y = 15000, family = "Colonna MT", size = 7, color = "#312016",
    label = "The Elder \nScrolls Online"
  ) +
  annotate(
    "text", x = 2020, y = 2000, family = "Colonna MT", size = 7, color = "#312016",
    label = "The Elder Scrolls IV: Oblivion"
  ) +
  annotate(
    "text", x = 2020, y = 0, family = "Colonna MT", size = 7, color = "#312016",
    label = "The Elder Scrolls III: Morrowind"
  ) + 
  scale_x_continuous(breaks = seq(2014, 2021, 1)) +
  theme_classic() +
  theme(axis.line = element_line(color = "#312016"),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = "#312016"),
        axis.text.y = element_text(colour = "#312016"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#dec29b", color = "#dec29b"),
        plot.background = element_rect(fill = "#dec29b", color = "#dec29b"),
        text = element_text(size = 18, family = "Colonna MT", color = "#312016"),
        plot.title = element_markdown(hjust = 0.5, size = 26),
        plot.subtitle = element_markdown(hjust = 0.5, size = 18),
        plot.caption = element_markdown(hjust = 0.5, size = 14),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))

