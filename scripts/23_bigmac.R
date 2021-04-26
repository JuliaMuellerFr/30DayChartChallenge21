# Day 23: Tiles

library(tidyverse)
library(worldtilegrid)
library(extrafont)

bigmac <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')

bigmac %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(year == "2010" | year == "2020") %>% 
  ggplot() +
  aes(country = iso_a3, fill = dollar_price) +
  geom_wtg(border_size = 0.5) +
  geom_text(
    aes(
      label = stat(alpha.2), 
      ), 
    stat = "wtg", size = 2
  ) +
  coord_equal() +
  facet_wrap(~ year) +
  scale_fill_viridis_c(
    na.value = alpha(hrbrthemes::ft_cols$gray, 1/5), direction = -1,
    option = "magma"
  ) +
  guides(
    fill = guide_colourbar(title.position = "top", title.hjust = 0.5)
  ) +
  labs(title = "The Big Mac Index",
       subtitle = "Data source: The Economist, via Tidy Tuesday",
       fill = "Price of a Big Mac in dollars") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 12, family = "Segoe UI Light", color = "gray30"),
        plot.title = element_text(hjust = 0.5, size = 22),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))

