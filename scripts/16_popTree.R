# Day 16 - Trees

library(rpart)
library(rpart.plot)
library(extrafont)
library(tidyverse)

set.seed(248)

spotify <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

spotify_pop <- spotify %>% 
  filter(playlist_genre == "pop") %>% 
  mutate(across(where(is.numeric), scale))

model_pop <- rpart(playlist_subgenre ~ track_popularity + danceability + energy + loudness + speechiness + acousticness + instrumentalness + liveness + valence + tempo + duration_ms, data = spotify_pop)

rpart.plot(model_pop, 
           type = 5, 
           extra = 104,
           box.palette = list(
             light_green = "#ADDDCE",
             green = "#70AE98",
             yellow = "#E6B655",
             orange = "#F0A35E"),
           leaf.round = 0,
           fallen.leaves = FALSE, 
           branch = 0.3, 
           under = TRUE,
           under.col = 'grey40',
           family = 'Myanmar Text',
           main = 'Decision tree: Pop subgenres by acoustic features\nData from Spotify via Tidy Tuesday',
           tweak = 1.4)
