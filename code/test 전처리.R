seosan1<- read_csv("????2016-2019(min).csv")
names(seosan1) <- c("date", "temp129", "rain_cum129", "wind_dir129", "wind_speed129", "spot_press129",
                    "sea_press129", "humid129", "solar_rad129", "solar_amount129")
seosan1$date<- as.character(seosan1$date)

seosan2<- read.csv("????2020(min).csv")
seosan2<- seosan2[,-(1:3)]
names(seosan2) <- c("date", "temp129", "rain_cum129", "wind_dir129", "wind_speed129", "spot_press129",
                    "sea_press129", "humid129", "solar_rad129", "solar_amount129")
seosan2$date<- as.character(seosan2$date)
seosan2$date<- ymd_hm(seosan2$date)

seosan1<- as.data.frame(seosan1)
seosan<- rbind(seosan1,seosan2)

# ????
sinpyeong1<- read_csv("????2016-2019(min).csv")
sinpyeong1<- sinpyeong1[,-(10:11)]
sinpyeong1$date<- as.character(sinpyeong1$date)
sinpyeong1$date<- ymd_hms(sinpyeong1$date)

sinpyeong2<- read.csv("????2020(min).csv")
sinpyeong2<- sinpyeong2[,-(1:3)]
names(sinpyeong2) <- c("date", "temp637", "rain637", "rain_status637", "wind_dir637", "wind_speed637",
                       "spot_press637", "sea_press637", "humid637")
sinpyeong2$date<- as.character(sinpyeong2$date)
sinpyeong2$date<- ymd_hm(sinpyeong2$date)

sinpyeong<- rbind(sinpyeong1, sinpyeong2)


# ????
dangjin1<- read.csv("????2016-2019(min).csv")
dangjin1<- dangjin1[,-(10:11)]
dangjin1$date<- as.character(dangjin1$date)
dangjin1$date<- ymd_hms(dangjin1$date)

dangjin2<- read.csv("????2020(min).csv")
dangjin2<- dangjin2[,-(1:3)]
names(dangjin2) <- c("date", "temp616", "rain616", "rain_status616", "wind_dir616", "wind_speed616",
                     "spot_press616", "sea_press616", "humid616")
dangjin2$date<- as.character(dangjin2$date)
dangjin2$date<- ymd_hm(dangjin2$date)

dangjin<- rbind(dangjin1, dangjin2)

## test?? ?ܺε????? join
plant_test<- read_csv("plant_test.csv")
plant_test$mea_ddhr<- as.character(plant_test$mea_ddhr)
plant_test$mea_ddhr<- ymd_hms(plant_test$mea_ddhr)

plant_test<- left_join(left_join(left_join(plant_test, seosan, by = c("mea_ddhr" = "date")), 
                                 dangjin, by = c("mea_ddhr" = "date")), 
                       sinpyeong, by = c("mea_ddhr" = "date"))

## test?? forecast join
forecast <- read_csv("forecast_final.csv")

f24 <- forecast %>% filter(forecast == 25)
f24 <- f24 %>%  mutate(date = fore_date - hours(24)) %>% select(-forecast, -DT, -KST, -fore_date)

for(i in 0:17){
  f24[paste0("date", i)] = f24$date + minutes(i * 10)
}

f24 <- f24 %>% select(-date) %>% gather(key = "idx", value = "date", date0:date17) %>% select(- idx)
names(f24) <- c("humid24", "rain_prob24", "sky_status24", "temp_3hour24", "wind_dir24", "wind_speed24", "date")


f48 <- forecast %>% filter(forecast == 49)
f48 <- f48 %>%  mutate(date = fore_date - hours(48)) %>% select(-forecast, -DT, -KST, -fore_date)
f48 <- rbind(f48 %>% filter(hour(date) == 00) %>% mutate(date = date + hours(3)), f48)

for(i in 0:17){
  f48[paste0("date", i)] = f48$date + minutes(i * 10)
}

f48 <- f48 %>% select(-date) %>% gather(key = "idx", value = "date", date0:date17) %>% select(- idx)
names(f48) <- c("humid48", "rain_prob48", "sky_status48", "temp_3hour48", "wind_dir48", "wind_speed48", "date")


plant_test <- left_join(left_join(plant_test, f24, by = c("mea_ddhr" = "date")), f48, by = c("mea_ddhr" = "date"))

## test?? df ?Ļ????? join
# ?̽?�� ???? ?Լ? ---------------------------------------------------------------
dpF<- function(rh, t){
  temp <- log(rh / 100) + 17.62 * t / (243.12 + t)
  dp <- 243.12 * temp / (17.62 - temp)
  return(dp)
}

data <- plant_test
# location
data["dp_loc"] <- dpF(data["hum_in"], data["tem_in"])

# out
data["dp_out"] <- dpF(data$hum_out_loc1, data$tem_out_loc1)

# ?ܺε????? 
for(i in c(616, 129)){
  data[paste0("dp", i)] <- dpF(data[paste0("humid",i)], data[paste0("temp", i)])
}

# ??????????
data["dp24"] <- dpF(data$humid24, data$temp_3hour24)
data["dp48"] <- dpF(data$humid48, data$temp_3hour48)

plant_test_joined <- data



## NAó??
idx<- which(is.na(plant_test_joined$temp129))
plant_test_joined[idx,"temp129"]<- plant_test_joined[idx,"temp616"]

idx<- which(is.na(plant_test_joined$wind_dir129))
plant_test_joined[idx,"wind_dir129"]<- plant_test_joined[idx,"wind_dir616"]

idx<- which(is.na(plant_test_joined$wind_speed129))
plant_test_joined[idx,"wind_speed129"]<- plant_test_joined[idx,"wind_speed616"]

idx<- which(is.na(plant_test_joined$spot_press129))
plant_test_joined[idx,"spot_press129"]<- plant_test_joined[idx,"spot_press637"]
plant_test_joined[idx,"spot_press616"]<- plant_test_joined[idx,"spot_press637"]

idx<- which(is.na(plant_test_joined$sea_press129))
plant_test_joined[idx,"sea_press129"]<- plant_test_joined[idx,"sea_press637"]
plant_test_joined[idx,"sea_press616"]<- plant_test_joined[idx,"sea_press637"]

idx<- which(is.na(plant_test_joined$humid129))
plant_test_joined[idx,"humid129"]<- plant_test_joined[idx,"humid637"]

idx<- which(is.na(plant_test_joined$temp616))
plant_test_joined[idx,"temp616"]<- plant_test_joined[idx,"temp637"]

idx<- which(is.na(plant_test_joined$rain616))
plant_test_joined[idx,"rain616"]<- plant_test_joined[idx,"rain637"]
plant_test_joined[idx,"rain_status616"]<- plant_test_joined[idx,"rain_status637"]

idx<- which(is.na(plant_test_joined$rain616))
plant_test_joined[idx,"rain616"]<- 0
plant_test_joined[idx,"rain637"]<-0

idx<- which(is.na(plant_test_joined$rain_status616))
plant_test_joined[idx,"rain_status616"]<- 0

idx<- which(is.na(plant_test_joined$wind_dir616))
plant_test_joined[idx,"wind_dir616"]<- plant_test_joined[idx,"wind_dir637"]

idx<- which(is.na(plant_test_joined$wind_speed616))
plant_test_joined[idx,"wind_speed616"]<- plant_test_joined[idx,"wind_speed637"]
plant_test_joined[idx,"spot_press616"]<- plant_test_joined[idx,"spot_press637"]

idx<- which(is.na(plant_test_joined$spot_press616))
plant_test_joined[idx,"spot_press616"]<- plant_test_joined[idx,"spot_press637"]

idx<- which(is.na(plant_test_joined$spot_press616))
plant_test_joined[idx,"spot_press616"]<- plant_test_joined[idx,"spot_press129"]

idx<- which(is.na(plant_test_joined$sea_press616))
plant_test_joined[idx,"sea_press616"]<- plant_test_joined[idx,"sea_press637"]

idx<- which(is.na(plant_test_joined$sea_press616))
plant_test_joined[idx,"sea_press616"]<- plant_test_joined[idx,"sea_press129"]

idx<- which(is.na(plant_test_joined$humid616))
plant_test_joined[idx,"humid616"]<- plant_test_joined[idx,"humid129"]

idx<- which(is.na(plant_test_joined$temp637))
plant_test_joined[idx,"temp637"]<- plant_test_joined[idx,"temp616"]

idx<- which(is.na(plant_test_joined$rain637))
plant_test_joined[idx,"rain637"]<- plant_test_joined[idx,"rain616"]

idx<- which(is.na(plant_test_joined$rain_status637))
plant_test_joined[idx,"rain_status637"]<- plant_test_joined[idx,"rain_status616"]

idx<- which(is.na(plant_test_joined$wind_dir637))
plant_test_joined[idx,"wind_dir637"]<- plant_test_joined[idx,"wind_dir616"]

idx<- which(is.na(plant_test_joined$wind_speed637))
plant_test_joined[idx,"wind_speed637"]<- plant_test_joined[idx,"wind_speed616"]

idx<- which(is.na(plant_test_joined$spot_press637))
plant_test_joined[idx,"spot_press637"]<- plant_test_joined[idx,"spot_press616"]
plant_test_joined[idx,"sea_press637"]<- plant_test_joined[idx,"sea_press616"]
plant_test_joined[idx,"humid637"]<- plant_test_joined[idx,"humid616"]

idx<- which(is.na(plant_test_joined$humid637))
plant_test_joined[idx,"humid637"]<- plant_test_joined[idx,"humid616"]

# ?ܺε????? 
for(i in c(616, 129)){
  plant_test_joined[paste0("dp", i)] <- dpF(plant_test_joined[paste0("humid",i)], plant_test_joined[paste0("temp", i)])
}

# knn��?? NA ��??
library(DMwR)
data<- plant_test_joined
weather<- knnImputation(data[,13:37])

plant_test_joined$rain_cum129<- weather$rain_cum129
plant_test_joined$solar_rad129<- weather$solar_rad129
plant_test_joined$solar_amount129<- weather$solar_amount129

data<- plant_test_joined
data$mea_ddhr<- as.character(data$mea_ddhr)
data$mea_ddhr<- ymd_hms(data$mea_ddhr)

data<- data %>% mutate(month = month(mea_ddhr), day = day(mea_ddhr), hour = hour(mea_ddhr), min = minute(mea_ddhr))

data$season<-0 
for(i in 1:nrow(data)){
  if(data$month[i] %in% c(3,4,5)){
    data$season[i]<- 1
  } else if(data$month[i] %in% c(6,7,8)){
    data$season[i]<- 2
  } else if(data$month[i] %in% c(9,10,11)){
    data$season[i]<- 3
  } else {
    data$season[i]<- 4
  }
}

test_dat<- data

write_csv(test_dat, "test_dat.csv")

