# Day 26: Trend

library(tidyverse)
library(extrafont)

energyG <- read_csv('C:/Users/Lenovo/Desktop/energyG.csv')

energyG_sum <- energyG %>% 
  pivot_longer(cols = -type, names_to = "year", values_to = "amount") %>% 
  group_by(year, type) %>% 
  summarise(n = sum(amount)) %>%
  mutate(percentage = n / sum(n),
         percentage = percentage * 100) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year))

labels <- data.frame(
  year = 2005,
  pos = c(5, 12, 25, 41.5, 47, 65, 85),
  lab = c("renewable sources", "other", "nuclear energy", "mineral oil", "gas", "brown coal", "black coal")
)

energyG_sum %>% 
  ggplot() +
  geom_area(aes(x=year, y=percentage, fill = type),
            alpha = 0.6 , size = 1)  +
  geom_text(data = labels, aes(x = year, y = pos, label = lab),
            colour = "white", family = "Tw Cen MT Condensed", size = 8) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1990, 2018, 2)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(10, 100, 10),
                     label = function(x) {return(paste(x, "%"))},
                     sec.axis = dup_axis()) +
  coord_cartesian(ylim = c(0, 100), xlim = c(1990, 2019)) +
  labs(title = "How Germany generated electricity, 1990 - 2019",
       subtitle = "Some progress towards the 'Energiewende'... but still a long way to go!",
       x = NULL, y = NULL,
       caption = "Data source: Umweltbundesamt") +
  scale_fill_manual(values = c("black", "#3e2a14", "#72a4d4", "black", "darkgray", "black", "#128111")) +
  theme(axis.line = element_line(color = "black"),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "black", size = 14),
        axis.title = element_text(colour = "black", size = 16),
        panel.grid.major.y = element_line(colour = "gray"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        text = element_text(size = 12, family = "Tw Cen MT Condensed", color = "black"),
        plot.title = element_text(hjust = 0.5, size = 30),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(hjust = 0.5, size = 18),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        legend.position = "none")


# Source: https://www.umweltbundesamt.de/daten/energie/erneuerbare-konventionelle-stromerzeugung#zeitliche-entwicklung-der-bruttostromerzeugung  

