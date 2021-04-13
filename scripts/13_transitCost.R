# Day 13: Correlation

library(extrafont)
library(ggtext)
library(tidyverse)

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

transit_cost <- transit_cost %>% 
  drop_na(country, real_cost, length) %>% 
  mutate(real_cost = as.numeric(real_cost))

transit_cost %>% 
  group_by(country) %>% 
  filter(n() >= 5) %>%
  summarise(avg_ckm = mean(cost_km_millions)) %>% 
  ungroup() %>% 
  arrange(desc(avg_ckm))

'highest cost per km: US, VN
lowest cost per km: ES, KR
country with most projects: CN'

transit_5 <- transit_cost %>% 
  filter(country %in% c("US", "VN", "ES", "KR", "CN")) %>% 
  mutate(country = fct_recode(country,
                              "China" = "CN",
                              "Vietnam" = "VN",
                              "Spain" = "ES",
                              "South Korea" = "KR",
                              "USA" = "US"
                              ))

p <- ggplot() +
  geom_point(data = transit_5, aes(x = real_cost, y = length, colour = country), size = 3) +
  geom_smooth(data = transit_5, aes(x = real_cost, y = length, colour = country), 
              method = "lm", se = FALSE) +
  geom_smooth(data = transit_cost, aes(x = real_cost, y = length), 
              method = "lm", se = FALSE, colour = "#eeda9d", size = 2, linetype = 3) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 15000, 1000)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 80, 10)) +
  expand_limits(x = 0, y = 0) +
  coord_cartesian(ylim = c(0, 90), xlim = c(0, 15500)) +
  labs(x = "Cost in millions of USD",
       y = "Length of line in km",
       title = "Do longer urban rail projects always cost more?",
       subtitle = 'The data set includes information on transit lines built since the late 1990s from 50 countries. This graph shows the countries with <br>
           the highest number of projects - <b style = "color:#959599;">China</b> | the lowest cost per km - <b style = "color:#80a0c7;">Spain</b> and <b style = "color:#394165;">South Korea</b> | the highest cost per km - the <b style = "color:#a65041;">USA</b> and <b style = "color:#dca258;">Vietnam</b>',
       caption = "<br>**Data source:** Transit Costs Project (*transitcosts.com*) via Tidy Tuesday") +
  scale_colour_manual(values = c("#959599", "#80a0c7", "#394165", "#a65041", "#dca258")) +
  theme(axis.line = element_line(color = "#100f14"),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = "#100f14"),
        axis.text.y = element_text(colour = "#100f14"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#fcf9f0", color = "#fcf9f0"),
        plot.background = element_rect(fill = "#fcf9f0", color = "#fcf9f0"),
        text = element_text(size = 12, family = "Bahnschrift", color = "#100f14"),
        plot.title = element_markdown(hjust = 0, size = 24),
        plot.subtitle = element_markdown(hjust = 0, size = 16),
        plot.caption = element_markdown(hjust = 1, size = 14),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))

transit_5 %>% 
  arrange(cost_km_millions) %>% 
  select(cost_km_millions, country, city, line, end_year, cost, length)

p_text <- p + 
  annotate(
    "text", x = 13000, y = 35, family = "Bahnschrift", size = 4, color = "#100f14", lineheight = .9,
    label = "This line shows the relationship between \ncost and length of the line for the entire data.") +
  annotate(
    "text", x = 8700, y = 14, family = "Bahnschrift", size = 4, color = "#100f14", lineheight = .9,
    label = "Cost for projects in the US is all over the place \neven though the constructed lines are fairly short.") +
  annotate(
    "text", x = 2000, y = 46, family = "Bahnschrift", size = 4, color = "#100f14", lineheight = .9,
    label = "The project with the lowest cost per km is the\nBeijing Capital Airport Express, completed in 2008.") +
  annotate(
    "text", x = 13000, y = 10, family = "Bahnschrift", size = 4, color = "#100f14", lineheight = .9,
    label = "East Side Access (New York)\nPrice tag: 11000 million US dollars.")

arrows <-
  tibble(
    x1 = c(800, 8000, 13500, 11800),
    x2 = c(220, 8000, 13200, 11100),
    y1 = c(43, 10, 38, 10),
    y2 = c(30, 6, 44, 3)
  )

p_text +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.1, "inch")), size = 0.6,
    color = "darkgray", curvature = 0
  )

