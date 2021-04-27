# Day 25: Demographic

library(tidyverse)
library(extrafont)

student_debt <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')

student_debt_last <- student_debt %>% 
  filter(year == "2016")

student_debt %>% 
  ggplot + 
  aes(x = year, y = loan_debt, colour = race) +
  geom_point(size = 4) +
  geom_line(size = 1) +
  labs(title = "Average family student loan debt in the US for ages 25-55",
       y = "Loan debt normalised to 2016 dollars", x = "Year",
       caption = "Urban Institute & US Census via Tidy Tuesday") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1989, 2016, 3)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 14000, 2000),
                     label = function(x) {return(paste(x, "$"))},
                     sec.axis = dup_axis(
                       breaks = student_debt_last$loan_debt,
                       labels = student_debt_last$race,
                       name = NULL)) +
  coord_cartesian(ylim = c(0, 15000), xlim = c(1988, 2016.5)) +
  scale_color_manual(values = c("#c70039", "#ff5733", "#ffc305")) +
  theme(axis.line = element_line(color = "black"),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "black", size = 14),
        axis.title = element_text(colour = "black", size = 16),
        panel.grid.major.y = element_line(colour = "gray"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        text = element_text(size = 12, family = "Javanese Text", color = "black"),
        plot.title = element_text(hjust = 0.5, size = 24),
        plot.caption = element_text(hjust = 0.5, size = 12),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        legend.position = "none")

