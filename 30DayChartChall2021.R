# Libraries

library(viridis)
library(countrycode)
library(gggibbous)
library(waffle)
library(emojifont)
library(extrafont)
library(ggtext)
library(tidyverse)


# Day 1: Part-to-whole

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


# Day 2: Pictogram

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


# Day 3: Historical

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

