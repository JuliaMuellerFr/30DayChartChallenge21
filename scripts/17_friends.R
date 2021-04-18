# Day 16: Pop culture

library(tidyverse)
library(tidytext)
library(extrafont)
library(ggtext)
library(cowplot)

friends <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')

friends_tfidf <- friends %>% 
  filter(speaker %in% c("Rachel Green", "Ross Geller", "Chandler Bing", "Monica Geller", "Joey Tribbiani", "Phoebe Buffay")) %>% 
  unnest_tokens(word, text) %>% 
  group_by(speaker, word) %>% 
  count() %>% 
  ungroup() %>% 
  bind_tf_idf(word, speaker, n) %>% 
  group_by(speaker) %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(10, tf_idf) %>% 
  ungroup %>% 
  mutate(word = as_factor(word),
         word = reorder_within(word, tf_idf, speaker)) %>% 
  separate(word, into = c("word_clean", "ig"), sep = "___", remove = FALSE)

friends_plot <- friends_tfidf %>% 
  ggplot(aes(x = word, y = tf_idf, fill = speaker)) +
  geom_col(aes(alpha = tf_idf), show.legend = FALSE) +
  geom_text(aes(label = word_clean), 
            position=position_dodge(width=0.9), hjust = 1,
            colour = "black", family = "Berlin Sans FB"
            ) +
  labs(x = NULL, y = "tf-idf",
       title = "\"Okay, you know how people say that Tulsa is the Paris of Oklahoma?\"",
       subtitle = "<br>Characteristic words\\* per main character in<br>",
       caption = "<br>\\*10 words with the highest inverse document frequency<br>**Data source:** Emil Hvitfeldt's {friends} package via *Tidy Tuesday*") +
  facet_wrap(~ speaker, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  scale_fill_manual(values = c("#e71913", "#04a4cf", "#f7bb00", "#f7bb00", "#e71913", "#04a4cf")) +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = "black", size = 16),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    text = element_text(size = 12, family = "Berlin Sans FB", color = "black"),
    plot.title = element_markdown(hjust = 0.5, size = 24, colour = "black"),
    plot.subtitle = element_markdown(hjust = 0.4, size = 18),
    plot.caption = element_markdown(hjust = 0.5, size = 14),
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
    legend.position = "none",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(colour = 'black', size = 14, face = "bold")
  )

ggdraw() +
  draw_plot(friends_plot) +
  draw_image("https://download.logo.wine/logo/Friends/Friends-Logo.wine.png", scale = .3,
             x = 0.21, y = 0.348)

