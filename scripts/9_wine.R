# Day 9: Statistics

library(tidyverse)
library(ggforce)
library(extrafont)
library(ggtext)
library(tidytext)

colorfindr::get_colors("https://i.pinimg.com/originals/94/2c/65/942c6589df3222cf0c5bb115705395eb.jpg", top_n = 30)

wine <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

wine_g <- wine %>% 
  filter(country == "Germany") %>%
  group_by(variety) %>% 
  filter(n() > 23) %>% 
  ungroup() %>% 
  drop_na(price) %>%
  mutate(variety = fct_reorder(variety, price, .fun = max))

wine_g %>% 
  filter(variety == "Riesling") %>% 
  arrange(desc(price)) %>% 
  select(winery, designation, description) %>% print()

wine_g %>% 
  filter(designation == "Kiedrich Gräfenberg Trockenbeerenauslese" & price == 775) %>% 
  unnest_tokens(word, description) %>% 
  anti_join(stop_words) %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(desc(n))

wine_g %>% 
  ggplot() +
  aes(x = variety, y = price, colour = variety) +
  geom_violin(lwd = 1) +
  geom_sina() +
  labs(title = 'Price points for the most frequently produced **wine** by grape types in Germany',
       subtitle = "*Riesling* is simultaneously the most frequently enjoyed wine and includes the most expensive bottles!",
       x = NULL, y = "Price per bottle\n",
       caption = "<br>**Data source:** Wine Enthusiasts, Kaggle/Tidy Tuesday") +
  scale_color_manual(values = c("#c87271", "#f0ea92", "#6d0f1f", "#cb893b", "#581119")) +
  theme(
    legend.position = "none",
    axis.text.x = element_markdown(color = "black", size = 14),
    axis.text.y = element_markdown(color = "black", size = 14),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    text = element_text(size = 18, family = "Constantia", color = "black"),
    plot.title = element_markdown(hjust = 0.5, size = 26),
    plot.subtitle = element_markdown(hjust = 0.5, size = 18),
    plot.caption = element_markdown(hjust = 0.5, size = 14),
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm")
  ) +
  annotate(
    "text", x = 4, y = 570, family = "Constantia", size = 4.5, color = "darkgray", lineheight = .9,
    label = "Winery: \nRobert Weil, in Rheingau. \n\nDesignation: \nKiedrich Gräfenberg Trockenbeerenauslese.\n\nFrequent words used in the description: \nsweet, acidity, dusty, honey, mineral, tangerine, caramel") +
  geom_curve(
    aes(x = 4, y = 700, xend = 4.99, yend = 790),
    arrow = arrow(length = unit(0.1, "inch")), size = 0.6,
    color = "darkgray", curvature = -0.3
  )
  

