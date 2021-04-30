# Day 28: Future

library(tidyverse)
library(forecast)
library(zoo)
library(ggtext)
library(extrafont)

hawaii <- read_csv("C:/Users/Lenovo/Documents/GitHub/coding_club/co2_loa.csv")

hawaii <- hawaii %>% 
  mutate(month = as.yearmon(month, format = "%Y-%m"),
         month = as.Date(month))

hawaii_ts <- ts(hawaii$co2_conc, start = 1958, end = 2008, freq = 12)  # Specify start and end year, measurement frequency (monthly = 12)

hawaii_model <- window(x = hawaii_ts, start = c(1958), end = c(2000))
hawaii_test <- window(x = hawaii_ts, start = c(2000))

hawaii_auto <- ets(hawaii_model)
hawaii_mmm <- ets(hawaii_model, model = "MMM")
hawaii_zzz<- ets(hawaii_model, model = "ZZZ")
hawaii_mmm_damped <- ets(hawaii_model, model = "MMM", damped = TRUE)

hawaii_auto_fc <- forecast(hawaii_auto, h = 650)  # `h = 60` means that the forecast will be 60 time periods long, in our case a time period is one month
hawaii_mmm_fc <- forecast(hawaii_mmm, h = 650)
hawaii_zzz_fc <- forecast(hawaii_zzz, h = 650)
hawaii_mmm_damped_fc <- forecast(hawaii_mmm_damped, h = 650)

hawaii_auto_df <- cbind("Month" = rownames(as.data.frame(hawaii_auto_fc)), as.data.frame(hawaii_auto_fc))  # Creating a data frame
names(hawaii_auto_df) <- gsub(" ", "_", names(hawaii_auto_df))  # Removing whitespace from column names
hawaii_auto_df$Date <- as.Date(paste("01-", hawaii_auto_df$Month, sep = ""), format = "%d-%b %Y")  # prepending day of month to date
hawaii_auto_df$Model <- rep("ets")  # Adding column of model type

hawaii_mmm_df <- cbind("Month" = rownames(as.data.frame(hawaii_mmm_fc)), as.data.frame(hawaii_mmm_fc))
names(hawaii_mmm_df) <- gsub(" ", "_", names(hawaii_mmm_df))
hawaii_mmm_df$Date <- as.Date(paste("01-", hawaii_mmm_df$Month, sep = ""), format = "%d-%b %Y")
hawaii_mmm_df$Model <- rep("ets_mmm")

hawaii_zzz_df <- cbind("Month" = rownames(as.data.frame(hawaii_zzz_fc)), as.data.frame(hawaii_zzz_fc))
names(hawaii_zzz_df) <- gsub(" ", "_", names(hawaii_zzz_df))
hawaii_zzz_df$Date <- as.Date(paste("01-", hawaii_zzz_df$Month, sep = ""), format = "%d-%b %Y")
hawaii_zzz_df$Model <- rep("ets_zzz")

hawaii_mmm_damped_df <- cbind("Month" = rownames(as.data.frame(hawaii_mmm_damped_fc)), as.data.frame(hawaii_mmm_damped_fc))
names(hawaii_mmm_damped_df) <- gsub(" ", "_", names(hawaii_mmm_damped_df))
hawaii_mmm_damped_df$Date <- as.Date(paste("01-", hawaii_mmm_damped_df$Month, sep = ""), format = "%d-%b %Y")
hawaii_mmm_damped_df$Model <- rep("ets_mmm_damped")

# Combining into one data frame
forecast_hawaii <- rbind(hawaii_auto_df, hawaii_mmm_df, hawaii_zzz_df, hawaii_mmm_damped_df)

forecast_hawaii <- drop_na(forecast_hawaii)
forecast_hawaii$mod <- as_factor(forecast_hawaii$Model)

forecast_zzz <- forecast_hawaii %>% 
  filter(mod == "ets_zzz")

ggplot() +
    geom_line(data = hawaii, aes(x = month, y = co2_conc), colour = "#c43d16") +  # Plotting original data
    geom_line(data = forecast_zzz, aes(x = Date, y = Point_Forecast), colour = "#fcb500") +  # Plotting model forecasts
    theme_classic() +
  labs(x = "Year", y = "Carbon dioxide (ppm)",
       title = "Carbon dioxide measured at Mauna Loa Observatory, Hawaii",
       subtitle = '<b style = "color:#c43d16;">Measured values</b> vs. <b style = "color:#fcb500;">predicted values</b>') +
  theme(axis.ticks = element_blank(),
        axis.text = element_markdown(),
        axis.title = element_markdown(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        text = element_text(size = 14, family = "MS Reference Sans Serif", color = "black"),
        plot.title = element_markdown(size = 24),
        plot.subtitle = element_markdown(size = 22),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        legend.position = "none")
