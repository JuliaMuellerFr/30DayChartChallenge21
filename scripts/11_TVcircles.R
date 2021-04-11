# Day 6: Experimental

library(hrbrthemes)
library(circlepackeR)
library(data.tree)
library(tidyverse)

TV <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")

TV_seasons <- TV %>% 
  separate(genres, into = c("genre", "g2", "g3"), sep = ",") %>% 
  group_by(title) %>% 
  mutate(seasons = max(seasonNumber)) %>% 
  select(genre, title, seasons) %>% 
  distinct() %>% 
  mutate(pathString = paste("TV", genre, title, sep = "/")) %>% 
  as.Node()

circlepackeR(TV_seasons, size = "seasons", color_min = "#D5EAFF", color_max = "#001328")

TV_shares <- TV %>% 
  separate(genres, into = c("genre", "g2", "g3"), sep = ",") %>% 
  group_by(title) %>% 
  mutate(avg_share = mean(share)) %>% 
  select(genre, title, avg_share) %>% 
  distinct() %>% 
  mutate(pathString = paste("TV", genre, title, sep = "/")) %>% 
  as.Node()

circlepackeR(TV_shares, size = "avg_share", color_min = "#BDFFD3", color_max = "#002E0F")


