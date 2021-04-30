# Day 27: Educational

library(tidyverse)
library(tidycode)
library(ggeffects)
library(extrafont)

libraries_used <- "
library(gggibbous)
library(extrafont)
library(ggtext)
library(tidyverse)
library(waffle)
library(emojifont)
library(extrafont)
library(ggtext)
library(tidyverse)
library(extrafont)
library(ggtext)
library(tidyverse)
library(extrafont)
library(ggtext)
library(magick)
library(cowplot)
library(patchwork)
library(tidyverse)
library(extrafont)
library(ggtext)
library(tidyverse)
library(flametree)
library(ggtext)
library(extrafont)
library(tidyverse)
library(jsonlite)
library(purrr)
library(data.table)
library(tidyverse)
library(extrafont)
library(ggtext)
library(tidyverse)
library(ggforce)
library(extrafont)
library(ggtext)
library(tidytext)
library(treemap)
library(tidyverse)
library(extrafont)
library(hrbrthemes)
library(circlepackeR)
library(data.tree)
library(tidyverse)
library(extrafont)
library(tidyverse)
library(ggstream)
library(ggtext)
library(extrafont)
library(ggtext)
library(tidyverse)
library(tidyverse)
library(ggshadow)
library(extrafont)
library(ggtext)
library(trekcolors)
library(extrafont)
library(ggtext)
library(ggforce)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(extrafont)
library(tidyverse)
library(tidyverse)
library(tidytext)
library(extrafont)
library(ggtext)
library(cowplot)
library(circlize)
library(tidyverse)
library(extrafont)
library(tidyverse)
library(RColorBrewer)
library(tidyverse)
library(ggtext)
library(extrafont)
library(patchwork)
library(tidyverse)
library(gganimate)
library(extrafont)
library(ggtext)
library(tidyverse)
library(worldtilegrid)
library(extrafont)
library(tidyverse)
library(ggforce)
library(extrafont)
library(tidyverse)
library(extrafont)
library(tidyverse)
library(extrafont)
library(tidyverse)
library(tidycode)
library(ggeffects)
library(extrafont)
"

libraries_used <- matahari::dance_recital(libraries_used)

libraries_used <- libraries_used %>% 
  rename("command" = "expr") %>% 
  select(command) %>% 
  mutate(command = as.character(command),
         command = str_replace(command, "library\\(", ""),
         command = str_replace(command, "\\)", "")
         ) %>%
  count(command) %>% 
  mutate(command = fct_reorder(command, n),
         first = if_else(command %in% c("ggtext", "ggforce", "worldtilegrid", "waffle", 
                                        "trekcolors", "treemap", "rpart", "rpart.plot", 
                                        "jsonlite", "ggstream", "ggshadow", "gggibbous", 
                                        "gganimate", "flametree", "emojifont", "data.tree"), 
                         "new", "old"),
         first = as_factor(first),
         command.label = paste("<span style = 'color: ",
                         ifelse(first == "new", "#60935D", "#14342B"),
                         ";'>",
                         command,
                         "</span>", sep = ""),
         command.label = fct_reorder(command.label, n)) 

t <- ggplot() +
  labs(title = "Packages I've used (so far) <br>during **#30DayChartChallenge**",
       subtitle = '...many of which <b style = "color:#60935D;">are new to me,</b> <br>but also <b style = "color:#14342B;">plenty of old favourites</b><br>',
       caption = "Numbers indicate the number of scripts (out of 26) \nin which I loaded each package") +
  theme(text = element_text(size = 12, family = "Lucida Bright", color = "black"),
        axis.line = element_blank(),
        plot.title = element_markdown(hjust = 0.5, size = 32),
        plot.subtitle = element_markdown(hjust = 0.5, size = 20),
        plot.caption = element_text(hjust = 0.5, size = 16),
        panel.background = element_blank(),
        plot.background = element_blank())

libraries_used %>% 
  ggplot() +
  aes(y = command.label, x = n, colour = first) +
  geom_point(aes(size = n), shape = 18) +
  geom_text(aes(label = n),
            colour = "white", family = "Lucida Bright", size = 4.5) +
  geom_segment(aes(y = command.label, yend = command.label, x = 0, xend = n)) +
  scale_colour_manual(values = c("#14342B", "#60935D")) +
  scale_size(range = c(8, 14)) +
  coord_cartesian(xlim = c(0, 26), ylim = c(-1, 31), expand = F) +
  labs(x = NULL, y = NULL) +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_markdown(size = 14, hjust = 1),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        text = element_text(size = 12, family = "Lucida Bright", color = "black"),
        plot.title = element_markdown(hjust = 0.5, size = 24),
        plot.subtitle = element_markdown(hjust = 0.5, size = 18),
        plot.margin = margin(t = 0.5, r = 1, b = 0, l = 1, unit = "cm"),
        legend.position = "none") +
  annotation_custom(ggplotGrob(t), xmin = 12, xmax = 14, ymin = 12, ymax = 14)

