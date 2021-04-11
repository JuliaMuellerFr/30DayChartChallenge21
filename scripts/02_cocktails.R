# Day 2: Pictogram

library(waffle)
library(emojifont)
library(extrafont)
library(ggtext)
library(tidyverse)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

classic_cocktails <- cocktails %>% 
  filter(category == "Cocktail Classics") %>% 
  mutate(ingredient = fct_collapse(ingredient, 
                                   "Lemon Juice" = c("Lemon Juice", "Juice of a Lemon"),
                                   "Lime Juice" = c("Lime Juice", "Juice of a Lime"),
                                   "Egg" = c("Egg White", "Egg"),
                                   "Sugar" = c("Sugar", "Powdered Sugar"),
                                   "Vermouth" = c("Dry Vermouth", "Sweet Vermouth")
  )) %>% 
  count(ingredient) %>% 
  filter(n > 40) %>% 
  arrange(desc(n))

classic_cocktails %>% 
  ggplot(aes(label = ingredient, values = n)) +
  geom_pictogram(n_rows = 10, aes(colour = ingredient), flip = FALSE, make_proportional = TRUE) +
  scale_color_manual(
    name = NULL,
    values = c("#b4643c", "#ffffff", "#ffffff", "#ffcf00", "#ffffff", "#59260b", "#8b0000", "#63B125"),
    labels = c("Vermouth", "Egg", "Gin", "Lemon Juice", "Sugar", "Brandy", "Grenadine", "Lime Juice")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("wine-bottle", "egg", "wine-bottle", "lemon", "candy-cane", "wine-bottle", "wine-bottle", "lemon"),
    labels = c("Vermouth", "Egg", "Gin", "Lemon Juice", "Sugar", "Brandy", "Grenadine", "Lime Juice")
  ) +
  coord_equal() +
  labs(title = 'The eight most popular ingredients in classic cocktails',
       subtitle = '<br>The most popular spirits are <b style = "color:#59260b;">Brandy,</b> <b style = "color:#ffffff;">Gin</b> and <b style = "color:#b4643c;">Vermouth.</b> <b style = "color:#8b0000;">Grenadine</b> is also a staple.',
       caption = "**Data source:** Mr. Boston Bartender's Guide via Tidy Tuesday"
  ) +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        text = element_text(size = 12, family = "Bahnschrift", color = "white"),
        plot.title = element_markdown(hjust = 0.5, size = 18),
        plot.subtitle = element_markdown(hjust = 0.5, size = 12),
        plot.caption = element_markdown(hjust = 0.5, size = 10),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))

