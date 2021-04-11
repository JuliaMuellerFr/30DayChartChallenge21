# Day 10: Abstract

library(treemap)
library(tidyverse)
library(extrafont)

artwork <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')

artwork %>% 
  group_by(medium) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n > 350) %>% 
  treemap(
        index = "medium",
        vSize = "n",
        type = "index",
        algorithm="squarified",
        border.col = "black",
        border.lwds = 4,
        fontsize.title = 24,
        inflate.labels = TRUE,
        title = "Media used in the \nTate Art Museum's Collection",
        fontfamily.title = "Copperplate Gothic Bold",
        fontfamily.labels = "Copperplate Gothic Light",
        palette = c("#d4d4d4", "#e1dddc", "#2d2f79", "#edbb34", "#e1dddc", "#f24439", "#d4d4d4", "#e1dddc", "#000000"),
        aspRatio = 8/9
  )

