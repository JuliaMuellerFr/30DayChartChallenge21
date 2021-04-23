library(extrafont)
library(tidyverse)
library(RColorBrewer)

theme_strip <- theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 3),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14, family = "High Tower Text", color = "black"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.caption = element_text(size = 12),
        panel.border = element_rect(colour = "#052517", fill = NA, size = 2),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm")
  )

col_strip <- brewer.pal(9, "YlGn")

key_crop_yields <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv',
                            col_names = c("country", "code", "year", "wheat", "rice", "maize", "soybeans", "potatoes", "beans", "peas", "cassava", "barley", "cocoa", "bananas"),
                            skip = 1)

yearly_yields <- key_crop_yields %>% 
  select(-c(country, code)) %>% 
  rowwise() %>% 
  mutate(total = sum(c(wheat, rice, maize, soybeans, potatoes, beans, peas, cassava, barley, cocoa, bananas), na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_longer(cols = -year, names_to = "crop", values_to = "amount") %>% 
  group_by(year) %>% 
  summarise(total = sum(amount, na.rm = TRUE)) %>% 
  ungroup()

yearly_yields %>% 
  ggplot(aes(x = year, y = 1, fill = total))+
  geom_tile(show.legend = FALSE) +
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = col_strip) +
  labs(title = "Global crop yields 1961 - 2018",
       caption = "Data source: Our World in Data via Tidy Tuesday") + 
  scale_x_continuous(expand = c(0, 0), breaks = seq(1960, 2015, 5)) +
  coord_cartesian(xlim = c(1961, 2018)) +
  theme_strip
