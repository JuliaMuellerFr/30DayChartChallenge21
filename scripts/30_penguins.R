# Day 30: 3D

library(tidyverse)
library(rgl)

penguins <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv') %>% 
  mutate(species = as_factor(species))

mycolors <- c('#f4d75e', '#e9723d', '#0b7fab')
penguins$color <- mycolors[as.numeric(penguins$species)]

par(mar=c(0,0,0,0))

plot3d( 
  x=penguins$bill_length_mm, y=penguins$bill_depth_mm, z=penguins$flipper_length_mm, 
  col = penguins$color, 
  type = 's', 
  radius = .4,
  xlab = "Bill length", ylab = "Bill depth", zlab = "Flipper length",
  main = "Palmer penguins - Adelie in yellow, Gentoo in orange, and Chinstrap in blue")
