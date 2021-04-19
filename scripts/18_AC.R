# Day 18: Connections

library(circlize)
library(tidyverse)

villagers <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

villagers <- villagers %>% 
  mutate(personality = as_factor(personality),
         species = as_factor(species)) %>% 
  group_by(species) %>%
  filter(n() > 15) %>% 
  ungroup() %>% 
  group_by(personality, species) %>% 
  count() %>% 
  ungroup()

grid.col = c(smug = "#f5cd47",
             jock = "#d1ebfa",
             snooty = "#17a577", 
             normal = "#b7413f", 
             lazy = "#eadb1c",
             uchi = "#6da9dd", 
             peppy = "#384178", 
             cranky = "#f27f50",
             cat = "gray", cub = "gray", dog = "grey", duck = "grey", 
             frog = "grey", rabbit = "grey", squirrel = "grey")

chordDiagram(villagers, 
             link.sort = TRUE, 
             link.decreasing = TRUE, 
             grid.col = grid.col, 
             transparency = 0.3, 
             annotationTrack = c("name","grid"))

circos.clear()

