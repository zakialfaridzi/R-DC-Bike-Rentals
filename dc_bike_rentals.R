library(tidyverse)
library(tidymodels)
library(dsbox)

glimpse(dcbikeshare)

# hard recode the data to align with the desc
dcbikeshare <- dcbikeshare %>%
  mutate(
    season = case_when(
      season == 1 ~ "winter",
      season == 2 ~ "spring",
      season == 3 ~ "summer",
      season == 4 ~ "fall"
    ),
    season = fct_relevel(season, "spring", "summer", "fall", "winter")
  )

# glimpse(dcbikeshare)

dcbikeshare <- dcbikeshare %>%
  mutate(
    holiday = ifelse(holiday == 0, "no", "yes"),      
    holiday = fct_relevel(holiday, "no", "yes"),    
    workingday = ifelse(workingday == 0, "no", "yes"),
    workingday = fct_relevel(workingday, "no", "yes")
  )
  
dcbikeshare <- dcbikeshare %>%
  mutate(
    yr = ifelse(yr == 0, "2011", "2012"),
    yr = fct_relevel(yr, "2011", "2012")
  )

dcbikeshare <- dcbikeshare %>%
  mutate(
    weathersit = case_when(
      weathersit == 1 ~ "clear",
      weathersit == 2 ~ "mist",
      weathersit == 3 ~ "light precipitation",
      weathersit == 4 ~ "heavy precipitation"
    ),
    weathersit = fct_relevel(weathersit, "clear", "mist", "light precipitation", "heavy precipitation")
  )

dcbikeshare <- dcbikeshare %>%
  mutate(
    temperature_raw = temp * 41,
    feeling_temperature_raw = atemp * 50,
    humidity_raw = hum * 100,
    windspeed_raw = windspeed * 67
  )

# make sure that if casual and registered columns are summed, the results are the same as cnt column
dcbikeshare %>% 
  mutate(casual_plus_registered = casual+registered) %>% 
  summarise(all_zero = all(casual_plus_registered == cnt))

# dataviz of the connection of warmer temp to the increase of bike rentals
ggplot(data = dcbikeshare) +
  geom_point(mapping = aes(x = dteday, y=cnt, color = feeling_temperature_raw), alpha = 0.7)+
  labs(
    title = "Rental Sepeda di Washington D.C. Tahun 2011 dan 2012",
    subtitle = "Suhu yang lebih hangat memiliki hubungan dengan kenaikan rental sepeda",
    x = "Tanggal",
    y = "Jumlah Rental Sepeda",
    color = "Suhu (C)"
  ) +
  theme_minimal()

# fit linear regression to predict bike rentals if certain temperature is met
cnt_temp_predict <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(cnt~temperature_raw, data=dcbikeshare)

cnt_temp_predict %>% 
  tidy()

glance(cnt_temp_predict)$r.squared

cnt_atmp <- linear_reg() %>%
  set_engine("lm") %>%
  fit(cnt ~ feeling_temperature_raw, data = dcbikeshare)

cnt_atmp %>%
  tidy()

glance(cnt_atmp)$r.squared


cnt_full <- linear_reg() %>%
  set_engine("lm") %>%
  fit(cnt ~ season + yr + holiday + workingday + weathersit +
        temperature_raw + feeling_temperature_raw + humidity_raw +
        windspeed_raw + feeling_temperature_raw * holiday, 
      data = dcbikeshare)
tidy(cnt_full)

glance(cnt_full)$r.squared