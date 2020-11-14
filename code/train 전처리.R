library(tidyverse)
library(lubridate)
setwd("C:\\Users\\조성관\\이화여자대학교\\513호 세입자 - 문서\\제출용\\data")
# read data -----------------------------------------------------------
data616 <- read_csv("당진2016-2019(min).csv")
data637 <- read_csv("신평2016-2019(min).csv")
data129 <- read_csv("서산2016-2019(min).csv") 
train1 <- read_csv("plant1_train.csv")
train2 <- read_csv("plant2_train.csv")
forecast <- read_csv("forecast_final.csv")

# join  ---------------------------------------------------------------
train1$mea_ddhr <- ymd_hm(train1$mea_ddhr)
train2$mea_ddhr <- ymd_hm(train2$mea_ddhr)

train1 <- left_join(left_join(left_join(train1, data129, by = c("mea_ddhr" = "date")), 
                              data616, by = c("mea_ddhr" = "date")), 
                    data637, by = c("mea_ddhr" = "date"))
train2 <- left_join(left_join(left_join(train2, data129, by = c("mea_ddhr" = "date")), 
                              data616, by = c("mea_ddhr" = "date")), 
                    data637, by = c("mea_ddhr" = "date"))

train1 <- train1 %>% select(-spot_press616, -sea_press616, -solar_rad616, -solar_amount616, -humid637,-solar_rad637, -solar_amount637)
train2 <- train2 %>% select(-spot_press616, -sea_press616, -solar_rad616, -solar_amount616, -humid637,-solar_rad637, -solar_amount637)


# y24, y48 ----------------------------------------------------------------
# plant1 y24
y1 <- train1 %>% select(mea_ddhr, cond_loc1) %>% rename('X24cond_loc1' = 'cond_loc1') %>% mutate(mea_ddhr = mea_ddhr - hours(24))
y2 <- train1 %>% select(mea_ddhr, cond_loc2) %>% rename('X24cond_loc2' = 'cond_loc2') %>% mutate(mea_ddhr = mea_ddhr - hours(24))
y3 <- train1 %>% select(mea_ddhr, cond_loc3) %>% rename('X24cond_loc3' = 'cond_loc3') %>% mutate(mea_ddhr = mea_ddhr - hours(24))
train1 <- left_join(left_join(left_join(train1, y1), y2), y3)

# plant2 y24 
y1 <- train2 %>% select(mea_ddhr, cond_loc1) %>% rename('X24cond_loc1' = 'cond_loc1') %>% mutate(mea_ddhr = mea_ddhr - hours(24))
y2 <- train2 %>% select(mea_ddhr, cond_loc2) %>% rename('X24cond_loc2' = 'cond_loc2') %>% mutate(mea_ddhr = mea_ddhr - hours(24))
y3 <- train2 %>% select(mea_ddhr, cond_loc3) %>% rename('X24cond_loc3' = 'cond_loc3') %>% mutate(mea_ddhr = mea_ddhr - hours(24))
train2 <- left_join(left_join(left_join(train2, y1), y2), y3)

# plant1 y48 
y1 <- train1 %>% select(mea_ddhr, cond_loc1) %>% rename('X48cond_loc1' = 'cond_loc1') %>% mutate(mea_ddhr = mea_ddhr - hours(48))
y2 <- train1 %>% select(mea_ddhr, cond_loc2) %>% rename('X48cond_loc2' = 'cond_loc2') %>% mutate(mea_ddhr = mea_ddhr - hours(48))
y3 <- train1 %>% select(mea_ddhr, cond_loc3) %>% rename('X48cond_loc3' = 'cond_loc3') %>% mutate(mea_ddhr = mea_ddhr - hours(48))
train1 <- left_join(left_join(left_join(train1, y1), y2), y3)

# plant2 y48 
y1 <- train2 %>% select(mea_ddhr, cond_loc1) %>% rename('X48cond_loc1' = 'cond_loc1') %>% mutate(mea_ddhr = mea_ddhr - hours(48))
y2 <- train2 %>% select(mea_ddhr, cond_loc2) %>% rename('X48cond_loc2' = 'cond_loc2') %>% mutate(mea_ddhr = mea_ddhr - hours(48))
y3 <- train2 %>% select(mea_ddhr, cond_loc3) %>% rename('X48cond_loc3' = 'cond_loc3') %>% mutate(mea_ddhr = mea_ddhr - hours(48))
train2 <- left_join(left_join(left_join(train2, y1), y2), y3)


# join forecast -----------------------------------------------------------
# for y24
f24 <- forecast %>% filter(forecast == 25)
f24 <- f24 %>%  mutate(date = fore_date - hours(24)) %>% select(-forecast, -DT, -KST, -fore_date)

# by 10 mins
for(i in 0:17){
  f24[paste0("date", i)] = f24$date + minutes(i * 10)
}
f24 <- f24 %>% select(-date) %>% gather(key = "idx", value = "date", date0:date17) %>% select(- idx)
names(f24) <- c("humid24", "rain_prob24", "sky_status24", "temp_3hour24", "wind_dir24", "wind_speed24", "date")


# for y48
f48 <- forecast %>% filter(forecast == 49)
f48 <- f48 %>%  mutate(date = fore_date - hours(48)) %>% select(-forecast, -DT, -KST, -fore_date)
f48 <- rbind(f48 %>% filter(hour(date) == 00) %>% mutate(date = date + hours(3)), f48)

# by 10 mins
for(i in 0:17){
  f48[paste0("date", i)] = f48$date + minutes(i * 10)
}
f48 <- f48 %>% select(-date) %>% gather(key = "idx", value = "date", date0:date17) %>% select(- idx)
names(f48) <- c("humid48", "rain_prob48", "sky_status48", "temp_3hour48", "wind_dir48", "wind_speed48", "date")

# join
train1 <- left_join(left_join(train1, f24, by = c("mea_ddhr" = "date")), f48, by = c("mea_ddhr" = "date"))
train2 <- left_join(left_join(train2, f24, by = c("mea_ddhr" = "date")), f48, by = c("mea_ddhr" = "date"))


# make dp -----------------------------------------------------------------
# dp function
dpF<- function(rh, t){
  temp <- log(rh / 100) + 17.62 * t / (243.12 + t)
  dp <- 243.12 * temp / (17.62 - temp)
  return(dp)
}

# plant1 
data <- train1
# location
for(idx.L in 1:3){
  data[paste0("dp_loc", idx.L)] <- dpF(data[paste0("hum_in_loc", idx.L)], data[paste0("tem_in_loc", idx.L)])
}

# out
data["dp_out"] <- dpF(data$hum_out_loc1, data$tem_out_loc1)

# 외부데이터 
for(i in c(616, 129)){
  data[paste0("dp", i)] <- dpF(data[paste0("humid",i)], data[paste0("temp", i)])
}

# 예보데이터
data["dp24"] <- dpF(data$humid24, data$temp_3hour24)
data["dp48"] <- dpF(data$humid48, data$temp_3hour48)

train1 <- data

# plant2 
data <- train2
# location
for(idx.L in 1:3){
  data[paste0("dp_loc", idx.L)] <- dpF(data[paste0("hum_in_loc", idx.L)], data[paste0("tem_in_loc", idx.L)])
}

# out
data["dp_out"] <- dpF(data$hum_out_loc1, data$tem_out_loc1)

# 외부데이터 
for(i in c(616, 129)){
  data[paste0("dp", i)] <- dpF(data[paste0("humid",i)], data[paste0("temp", i)])
}

# 예보데이터
data["dp24"] <- dpF(data$humid24, data$temp_3hour24)
data["dp48"] <- dpF(data$humid48, data$temp_3hour48)

train2 <- data


# month, day, hour, min, season -------------------------------------------
# month, day, hour, min
train1 = train1 %>% mutate(month = month(mea_ddhr),
                               day = day(mea_ddhr),
                               hour = hour(mea_ddhr),
                               min = minute(mea_ddhr))

train2 = train2 %>% mutate(month = month(mea_ddhr),
                               day = day(mea_ddhr),
                               hour = hour(mea_ddhr),
                               min = minute(mea_ddhr))


# season 
train1$season <- ifelse(train1$month %in% c(3, 4, 5), 1, 0)
train1$season <- ifelse(train1$month %in% c(6, 7, 8), 2, train1$season)
train1$season <- ifelse(train1$month %in% c(9, 10, 11), 3, train1$season)
train1$season <- ifelse(train1$month %in% c(12, 1, 2), 4, train1$season)

train2$season <- ifelse(train2$month %in% c(3, 4, 5), 1, 0)
train2$season <- ifelse(train2$month %in% c(6, 7, 8), 2, train2$season)
train2$season <- ifelse(train2$month %in% c(9, 10, 11), 3, train2$season)
train2$season <- ifelse(train2$month %in% c(12, 1, 2), 4, train2$season)


# for 1 model -------------------------------------------------------------
# y24 plant1 
p1l1 <- train1 %>% select(-cond_loc1, -cond_loc2, -cond_loc3, -X48cond_loc1, -X48cond_loc2, -X48cond_loc3,
                          -X24cond_loc2, -tem_in_loc2, -hum_in_loc2, -tem_coil_loc2, -dp_loc2,
                          -X24cond_loc3, -tem_in_loc3, -hum_in_loc3, -tem_coil_loc3, -dp_loc3,
                          -humid48, -rain_prob48, -sky_status48, -temp_3hour48, -wind_dir48, -wind_speed48, -dp48) %>% 
  mutate(plant = 1, location = 1, time = 24)
p1l2 <- train1 %>% select(-cond_loc1, -cond_loc2, -cond_loc3, -X48cond_loc1, -X48cond_loc2, -X48cond_loc3,
                          -X24cond_loc1, -tem_in_loc1, -hum_in_loc1, -tem_coil_loc1, -dp_loc1,
                          -X24cond_loc3, -tem_in_loc3, -hum_in_loc3, -tem_coil_loc3, -dp_loc3,
                          -humid48, -rain_prob48, -sky_status48, -temp_3hour48, -wind_dir48, -wind_speed48, -dp48) %>% 
  mutate(plant = 1, location = 2, time = 24)
p1l3 <- train1 %>% select(-cond_loc1, -cond_loc2, -cond_loc3, -X48cond_loc1, -X48cond_loc2, -X48cond_loc3,
                          -X24cond_loc2, -tem_in_loc2, -hum_in_loc2, -tem_coil_loc2, -dp_loc2,
                          -X24cond_loc1, -tem_in_loc1, -hum_in_loc1, -tem_coil_loc1, -dp_loc1,
                          -humid48, -rain_prob48, -sky_status48, -temp_3hour48, -wind_dir48, -wind_speed48, -dp48) %>% 
  mutate(plant = 1, location = 2, time = 24)
name <- c('mea_ddhr', 'tem_in_loc', 'hum_in_loc', 'tem_coil_loc', 'tem_out_loc', 'hum_out_loc', 
          'temp129', 'rain_cum129', 'wind_dir129', 'wind_speed129', 'spot_press129', 'sea_press129', 'humid129', 'solar_rad129', 'solar_amount129', 
          'temp616', 'rain616', 'rain_status616', 'wind_dir616', 'wind_speed616', 'humid616', 
          'temp637', 'rain637', 'rain_status637', 'wind_dir637', 'wind_speed637', 'spot_press637', 'sea_press637', 
          'cond_loc', 'fore_humid', 'fore_rain_prob', 'fore_sky_status', 'fore_temp_3hour', 'fore_wind_dir', 'fore_wind_speed', 
          'dp_loc', 'dp_out', 'dp616', 'dp129', 'fore_dp',
          'month', 'day', 'hour', 'min', 'season', 'plant', 'location', 'time')
names(p1l1) <- name; names(p1l2) <- name; names(p1l3) <- name

# y24 plant2 
p2l1 <- train2 %>% select(-cond_loc1, -cond_loc2, -cond_loc3, -X48cond_loc1, -X48cond_loc2, -X48cond_loc3,
                          -X24cond_loc2, -tem_in_loc2, -hum_in_loc2, -tem_coil_loc2, -dp_loc2,
                          -X24cond_loc3, -tem_in_loc3, -hum_in_loc3, -tem_coil_loc3, -dp_loc3,
                          -humid48, -rain_prob48, -sky_status48, -temp_3hour48, -wind_dir48, -wind_speed48, -dp48) %>% 
  mutate(plant = 2, location = 1, time = 24)
p2l2 <- train2 %>% select(-cond_loc1, -cond_loc2, -cond_loc3, -X48cond_loc1, -X48cond_loc2, -X48cond_loc3,
                          -X24cond_loc1, -tem_in_loc1, -hum_in_loc1, -tem_coil_loc1, -dp_loc1,
                          -X24cond_loc3, -tem_in_loc3, -hum_in_loc3, -tem_coil_loc3, -dp_loc3,
                          -humid48, -rain_prob48, -sky_status48, -temp_3hour48, -wind_dir48, -wind_speed48, -dp48) %>% 
  mutate(plant = 2, location = 2, time = 24)
p2l3 <- train2 %>% select(-cond_loc1, -cond_loc2, -cond_loc3, -X48cond_loc1, -X48cond_loc2, -X48cond_loc3,
                          -X24cond_loc2, -tem_in_loc2, -hum_in_loc2, -tem_coil_loc2, -dp_loc2,
                          -X24cond_loc1, -tem_in_loc1, -hum_in_loc1, -tem_coil_loc1, -dp_loc1,
                          -humid48, -rain_prob48, -sky_status48, -temp_3hour48, -wind_dir48, -wind_speed48, -dp48) %>% 
  mutate(plant = 2, location = 3, time = 24)

names(p2l1) <- name; names(p2l2) <- name; names(p2l3) <- name
plant1 = rbind(p1l1, p1l2, p1l3, p2l1, p2l2, p2l3)

# y48 plant1 
p1l1 <- train1 %>% select(-cond_loc1, -cond_loc2, -cond_loc3, -X24cond_loc1, -X24cond_loc2, -X24cond_loc3,
                          -X48cond_loc2, -tem_in_loc2, -hum_in_loc2, -tem_coil_loc2, -dp_loc2,
                          -X48cond_loc3, -tem_in_loc3, -hum_in_loc3, -tem_coil_loc3, -dp_loc3,
                          -humid24, -rain_prob24, -sky_status24, -temp_3hour24, -wind_dir24, -wind_speed24, -dp24) %>% 
  mutate(plant = 1, location = 1, time = 48)
p1l2 <- train1 %>% select(-cond_loc1, -cond_loc2, -cond_loc3, -X24cond_loc1, -X24cond_loc2, -X24cond_loc3,
                          -X48cond_loc1, -tem_in_loc1, -hum_in_loc1, -tem_coil_loc1, -dp_loc1,
                          -X48cond_loc3, -tem_in_loc3, -hum_in_loc3, -tem_coil_loc3, -dp_loc3,
                          -humid24, -rain_prob24, -sky_status24, -temp_3hour24, -wind_dir24, -wind_speed24, -dp24) %>% 
  mutate(plant = 1, location = 2, time = 48)
p1l3 <- train1 %>% select(-cond_loc1, -cond_loc2, -cond_loc3, -X24cond_loc1, -X24cond_loc2, -X24cond_loc3,
                          -X48cond_loc2, -tem_in_loc2, -hum_in_loc2, -tem_coil_loc2, -dp_loc2,
                          -X48cond_loc1, -tem_in_loc1, -hum_in_loc1, -tem_coil_loc1, -dp_loc1,
                          -humid24, -rain_prob24, -sky_status24, -temp_3hour24, -wind_dir24, -wind_speed24, -dp24) %>% 
  mutate(plant = 1, location = 3, time = 48)
names(p1l1) <- name; names(p1l2) <- name; names(p1l3) <- name

# y48 plant2 
p2l1 <- train2 %>% select(-cond_loc1, -cond_loc2, -cond_loc3, -X24cond_loc1, -X24cond_loc2, -X24cond_loc3,
                          -X48cond_loc2, -tem_in_loc2, -hum_in_loc2, -tem_coil_loc2, -dp_loc2,
                          -X48cond_loc3, -tem_in_loc3, -hum_in_loc3, -tem_coil_loc3, -dp_loc3,
                          -humid24, -rain_prob24, -sky_status24, -temp_3hour24, -wind_dir24, -wind_speed24, -dp24) %>% 
  mutate(plant = 2, location = 1, time = 48)
p2l2 <- train2 %>% select(-cond_loc1, -cond_loc2, -cond_loc3, -X24cond_loc1, -X24cond_loc2, -X24cond_loc3,
                          -X48cond_loc1, -tem_in_loc1, -hum_in_loc1, -tem_coil_loc1, -dp_loc1,
                          -X48cond_loc3, -tem_in_loc3, -hum_in_loc3, -tem_coil_loc3, -dp_loc3,
                          -humid24, -rain_prob24, -sky_status24, -temp_3hour24, -wind_dir24, -wind_speed24, -dp24) %>% 
  mutate(plant = 2, location = 2, time = 48)
p2l3 <- train2 %>% select(-cond_loc1, -cond_loc2, -cond_loc3, -X24cond_loc1, -X24cond_loc2, -X24cond_loc3,
                          -X48cond_loc2, -tem_in_loc2, -hum_in_loc2, -tem_coil_loc2, -dp_loc2,
                          -X48cond_loc1, -tem_in_loc1, -hum_in_loc1, -tem_coil_loc1, -dp_loc1,
                          -humid24, -rain_prob24, -sky_status24, -temp_3hour24, -wind_dir24, -wind_speed24, -dp24) %>% 
  mutate(plant = 2, location = 3, time = 48)
names(p2l1) <- name; names(p2l2) <- name; names(p2l3) <- name
plant2 = rbind(p1l1, p1l2, p1l3, p2l1, p2l2, p2l3)

train <- rbind(plant1, plant2)

write_csv(train, "train_dat.csv")



