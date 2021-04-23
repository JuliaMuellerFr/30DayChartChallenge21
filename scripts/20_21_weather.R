# Days 20 Upwards & 21 Downwards

library(tidyverse)
library(ggtext)
library(extrafont)
library(patchwork)

theme_update(axis.line = element_line(color = "black"),
             axis.ticks.y = element_blank(),
             axis.ticks.x = element_blank(),
             axis.text.x = element_text(colour = "black"),
             axis.text.y = element_text(colour = "black"),
             panel.grid = element_blank(),
             panel.background = element_rect(fill = "white", color = "white"),
             plot.background = element_rect(fill = "white", color = "white"),
             text = element_text(size = 14, family = "Sylfaen", color = "black"),
             plot.title = element_markdown(hjust = 0.5, size = 24),
             plot.subtitle = element_markdown(hjust = 0.5, size = 16),
             plot.caption = element_markdown(hjust = 0.5, size = 10),
             legend.position = "none",
             )

weatheR <- read_delim("C:/Users/Lenovo/Desktop/produkt_klima_monat_19440701_20191231_04177.txt", delim = ";") 

weatheR <- weatheR %>% 
  mutate(MO_TT = str_trim(MO_TT),
         MO_RR = str_trim(MO_RR),
         MO_TX = str_trim(MO_TX),
         MESS_DATUM_BEGINN = str_trim(MESS_DATUM_BEGINN),
         MESS_DATUM_ENDE = str_trim(MESS_DATUM_ENDE),
         MO_TT = na_if(MO_TT, "-999"),
         MO_RR = na_if(MO_RR, "-999"),
         MO_TX = na_if(MO_TX, "-999"),
         MO_TT = as.numeric(MO_TT),
         MO_RR = as.numeric(MO_RR),
         MO_TX = as.numeric(MO_TX))

weatheR_sum <- weatheR %>% 
  mutate(MESS_DATUM_BEGINN = lubridate::ymd(MESS_DATUM_BEGINN),
         MESS_DATUM_ENDE = lubridate::ymd(MESS_DATUM_ENDE),
         year = lubridate::year(MESS_DATUM_ENDE)) %>% 
  group_by(year) %>% 
  filter(n() == 12) %>% 
  summarise(avg_temp = mean(MO_TT, na.rm = TRUE), highest_temp = max(MO_TX), avg_rain = mean(MO_RR, na.rm = TRUE)) %>% 
  ungroup() 

temp <- weatheR_sum %>% 
  ggplot() +
  aes(year, highest_temp) +
  geom_point(colour = "#940005") +
  geom_smooth(method = "lm", colour = "#5B0000", fill = "#5B0000") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1950, 2020, 5)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(22, 32, 1),
                     label = function(x) {return(paste(x, "°C"))}) +
  coord_cartesian(ylim = c(22, 32), xlim = c(1945, 2022)) +
  labs(x = "Year", y = NULL)

rain <- weatheR_sum %>% 
  ggplot() +
  aes(year, avg_rain) +
  geom_point(colour = "#00437F") +
  geom_smooth(method = "lm", colour = "#040069", fill = "#040069") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1950, 2020, 5)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(40, 110, 10),
                     label = function(x) {return(paste(x, "mm"))}) +
  coord_cartesian(ylim = c(42, 105), xlim = c(1945, 2022)) +
  labs(x = "Year", y = NULL)

weather_title <- ggplot() +
  labs(title = '<b style = "color:#5B0000;">Highest temperature recorded</b> and <b style = "color:#040069;">average rain</b> per year in Rheinstetten, Germany',
       subtitle = "<br>**Data source**: Deutscher Wetterdienst<br>Data missing between 1985 and 2008") +
  theme(axis.line = element_blank())

weather_title/(temp + rain) + 
  plot_layout(heights = c(0.2, 1, 1))

