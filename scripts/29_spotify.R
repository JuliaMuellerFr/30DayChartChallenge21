# Day 29: Deviations

library(tidyverse)
library(extrafont)
library(colorspace)
library(scico)

spotify <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

spotify_long <- spotify %>% 
  select(playlist_genre, danceability, energy, loudness, speechiness, liveness, valence) %>% 
  pivot_longer(cols = -playlist_genre, names_to = "characteristic", values_to = "rating") %>% 
  mutate(playlist_genre = recode(playlist_genre,
                                 edm = "EDM",
                                 latin = "Latin",
                                 pop = "Pop",
                                 'r&b' = "R&B",
                                 rap = "Rap",
                                 rock = "Rock"),
         characteristic = str_to_title(characteristic)
         ) 

spotify_long %>% 
  ggplot() +
  aes(x = playlist_genre, y = rating) +
  geom_violin(aes(colour = playlist_genre, fill = after_scale(alpha(colour, 0.4)))) +
  geom_boxplot(aes(colour = playlist_genre), width = 0.2, outlier.size = 1) +
  scale_color_scico_d(palette = "roma") +
  facet_wrap(~ characteristic, scales = "free") +
  labs(x = NULL, y = NULL,
       title = "Music genres and audio features on Spotify",
       caption = "Data source: {spotifyr} package via Tidy Tuesday") +
  theme(axis.line = element_line(colour = "darkgray"),
        axis.ticks = element_blank(),
        axis.text = element_markdown(),
        axis.title = element_markdown(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        text = element_text(size = 14, family = "OCR A Extended", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 12, hjust = 0.5),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        legend.position = "none",
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(colour = 'white', size = 14, face = "bold", family = "OCR A Extended"),
        panel.spacing = unit(2, "lines"))
