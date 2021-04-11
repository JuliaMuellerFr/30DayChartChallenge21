# Day 8: Animals

library(jsonlite)
library(purrr)
library(data.table)
library(tidyverse)
library(extrafont)
library(ggtext)

cats_js <- fromJSON("C:/Users/Lenovo/Desktop/cats.json", flatten = TRUE)
cats_list <- map(cats_js, as.data.table)
cats <- rbindlist(cats_list, fill = TRUE, idcol = T) %>% 
  rename("breed" = '.id')

cats_long <- cats %>% 
  add_row(breed = "Flocke", 
          'Affectionate with Family' = 5,
          'Amount of Shedding' = 3, 
          'Easy to Groom' = 4, 
          'General Health' = 5, 
          'Intelligence' = 5, 
          'Kid Friendly' = NA, 
          'Pet Friendly' = 2, 
          'Potential for Playfulness' = 3, 
          'Friendly Toward Strangers' = 2, 
          'Tendency to Vocalize' = 5) %>% 
  pivot_longer(cols = -breed, names_to = "characteristic", values_to = "rating") 

labels <- c(
  'Bengal Cats' = "<img src='C:/Users/Lenovo/Desktop/cats/bengal.jpg'
    width='120' /><br>**Bengal**",
  'Maine Coon' = "<img src='C:/Users/Lenovo/Desktop/cats/mainecoon.jpg'
    width='120' /><br>**Maine Coon**",
  'Norwegian Forest' = "<img src='C:/Users/Lenovo/Desktop/cats/norwegian.jpg'
    width='120' /><br>**Norwegian Forest**",
  'Scottish Fold' = "<img src='C:/Users/Lenovo/Desktop/cats/scottishfold.jpg'
    width='120' /><br>**Scottish Fold**",
  'Ragdoll Cats' = "<img src='C:/Users/Lenovo/Desktop/cats/ragdoll.jpg'
    width='120' /><br>**Ragdoll**",
  'Flocke' = "<img src='C:/Users/Lenovo/Desktop/cats/flocke.jpeg'
    width='120' /><br>**Flocke**"
)

cats_long %>% 
  filter(breed %in% c("Bengal Cats", "Maine Coon", "Norwegian Forest", "Scottish Fold", "Ragdoll Cats", "Flocke")) %>%
  drop_na() %>% 
  mutate(breed = fct_relevel(breed, "Flocke")) %>% 
  ggplot +
  aes(x = breed, y = characteristic, size = rating, colour = breed, alpha = rating) +
  geom_point() +
  scale_x_discrete(labels = labels) +
  scale_size_continuous(range = c(2, 12)) +
  labs(x = NULL, y = NULL,
       title = "My cat's the best!",
       subtitle = "Comparing **Flocke's** characteristics to other cat breeds",
       caption = "**Data source:** *cattime.com*, webscraped by Ruslan Baynazarov and uploaded to Kaggle. <br>Pictures via Wikimedia - files are in the public domain.") +
  scico::scale_color_scico_d(palette = "corkO") +
  theme(
    legend.position = "none",
    axis.text.x = element_markdown(color = "black", size = 14),
    axis.text.y = element_markdown(color = "black", size = 14),
    axis.line = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    text = element_text(size = 18, family = "Ink Free", color = "#51565c"),
    plot.title = element_markdown(hjust = 0.5, size = 26),
    plot.subtitle = element_markdown(hjust = 0.5, size = 18),
    plot.caption = element_markdown(hjust = 0.5, size = 14),
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm")
  )

