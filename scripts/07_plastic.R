# Day 7: Physical

options(scipen = 999)

library(ggtext)
library(extrafont)
library(tidyverse)

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

plastics_sum <- plastics %>% 
  pivot_longer(cols = c(o, pet, pp, ps, pvc, hdpe, ldpe), names_to = "plastic_type", values_to = "plastic_count") %>% 
  drop_na(plastic_count) %>% 
  group_by(year, plastic_type) %>% 
  summarise(sum(plastic_count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = year, values_from = `sum(plastic_count)`) %>% 
  mutate(plastic_type = as_factor(plastic_type),
         plastic_type = fct_relevel(plastic_type, "hdpe", after = Inf),
         plastic_type = fct_relevel(plastic_type, "ldpe", after = 2),
         plastic_type = fct_relevel(plastic_type, "hdpe", after = 4))

p <- plastics_sum %>% 
  ggplot() +
  aes(x = plastic_type) +
  geom_segment(aes(xend=plastic_type, 
                   y=`2019`, yend=`2020`), 
               color="grey") +
  geom_point(aes(y = `2019`), 
             size = 4, colour = "#FFA500", shape = 17
             ) +
  geom_point(aes(y = `2020`), 
             size = 4, colour = "#2C2CFF", shape = 15) +
  theme_minimal() +
  scale_x_discrete(labels = c('Other',
                              'Polyester\nplastic',
                              'Low density \npolyethylene\n',
                              'Polypropylene',
                              'Polystyrene',
                              'PVC',
                              'High density\npolyethylene'
                              )) +
  labs(x = "Type of plastic (recycling code)", 
       y = "Amount found\n",
       title = '**Plastic pollution** in <b style = "color:#FFA500;">2019</b> and <b style = "color:#2C2CFF;">2020</b>',
       subtitle = "Types of plastic found during *Break Free from Plastic's* global audits",
       caption = "<br>**Data Source:** Sarah Sauve, via Tidy Tuesday") +
  theme(axis.line = element_line(color = "#51565c"),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = "#51565c"),
        axis.text.y = element_text(colour = "#51565c"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        text = element_text(size = 18, family = "Lato Light", color = "#51565c"),
        plot.title = element_markdown(hjust = 0.5, size = 26),
        plot.subtitle = element_markdown(hjust = 0.5, size = 18),
        plot.caption = element_markdown(hjust = 0.5, size = 14),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))

p_text <- p +
  annotate(
    "text", x = 2, y = 350000, family = "Lato Light", size = 4, color = "#51565c", lineheight = .9,
    label = "Polyester fibers, \nsoft drink bottles, \nfood containers...") +
  annotate(
    "text", x = 3, y = 300000, family = "Lato Light", size = 4, color = "#51565c", lineheight = .9,
    label = "Plastic/Ziploc bags, \nsqueeze bottles, \nplastic tubes, \nchopping boards...") +
  annotate(
    "text", x = 4, y = 250000, family = "Lato Light", size = 4, color = "#51565c", lineheight = .9,
    label = "Flower pots, bumpers, \nmicrowavable food containers, \ncar interior trim, \nindustrial fibers, \ncarry-out beverage cups...") +
  annotate(
    "text", x = 5, y = 200000, family = "Lato Light", size = 4, color = "#51565c", lineheight = .9,
    label = "Plastic milk containers, \nplastic bags, bottle caps, \ntrash and oil cans, \ntoolboxes, \nsupplement containers...") +
  annotate(
    "text", x = 6, y = 150000, family = "Lato Light", size = 4, color = "#51565c", lineheight = .9,
    label = "Styrofoam, toys, \nvideo cassettes, \nbeverage/food coolers, \nbeer/wine/champagne cups, \ncarry-out food containers...") +
  annotate(
    "text", x = 7, y = 100000, family = "Lato Light", size = 4, color = "#51565c", lineheight = .9,
    label = "Window frames, \nbottles for chemicals, \nflooring, plumbing pipes...")

arrows <-
  tibble(
    x1 = c(2, 3, 4, 5, 6, 7),
    x2 = c(2.1, 3.1, 4.1, 5.1, 6.1, 7.1),
    y1 = c(320000, 250000, 200000, 150000, 100000, 70000),
    y2 = c(210000, 100000, 80000, 30000, 12000, 11000)
  )

p_text +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.1, "inch")), size = 0.6,
    color = "gray", curvature = -0.3
  )

