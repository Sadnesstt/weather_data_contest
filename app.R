## for publish ## 


rm(list = ls())
library(tidyverse)
library(lubridate)
library(reticulate)
# library(RSelenium)
# library(rvest)
# library(httr)
Sys.setlocale("LC_ALL", "C")
# ##### publish 할 때 주석 제거 #####
# reticulate::virtualenv_create('young1')
# reticulate::virtualenv_install('young1', packages = 'lightgbm')
# reticulate::virtualenv_install('young1', packages = 'requests')
# reticulate::virtualenv_install('young1', packages = 'pandas')
# reticulate::virtualenv_install('young1', packages = 'selenium.webdriver.support.ui')
# reticulate::virtualenv_install('young1', packages = 'numpy')
# 
# reticulate::use_virtualenv('young1', required = TRUE )
#######
##### publish 할 때 주석 씌우기 #####
# use_virtualenv('C:\\Users\\dnskd\\Desktop\\final\\young')
# setwd('C:\\Users\\dnskd\\Desktop\\final')
#######
lgb <- import('lightgbm')
# source_python('crawling for shiny.py')

# get datetime ------------------------------------------------------------
getTime <- function(){
  temp <- Sys.time()
  month = month(temp)
  if(month %in% c(12, 1, 2)){
    season = 4
  }else if(month %in% c(3, 4, 5)){
    season = 1
  }else if(month %in% c(6, 7, 8)){
    season = 2
  }else{
    season = 3
  }
  return(list(year = year(temp),month = month,
              day = day(temp), hour = hour(temp),
              min = minute(temp), season = season,
              time = temp))
}

# get dp ------------------------------------------------------------------
dpF<- function(rh, t){
  temp <- log(rh / 100) + 17.62 * t / (243.12 + t)
  dp <- 243.12 * temp / (17.62 - temp)
  return(dp)
}

# test_new 생성 ----------------------------------------------------------- # 46개
newData <- function(plant, tem_out, hum_out, tem_in1, tem_coil1, hum_in1,
                    tem_in2, tem_coil2, hum_in2, tem_in3, tem_coil3, hum_in3,
                    month, day, hour, min, season,
                    out_data){
  in_data = data.frame(plant = rep(plant, 6), tem_out_loc = rep(tem_out, 6), hum_out_loc = rep(hum_out, 6),
                       month = rep(month, 6), day = rep(day, 6), hour = rep(hour, 6), min = rep(min, 6),
                       season = rep(season, 6), location = rep(c(1,2,3), 2),
                       tem_in_loc = rep(c(tem_in1, tem_in2, tem_in3), 2),
                       tem_coil_loc = rep(c(tem_coil1, tem_coil2, tem_coil3), 2),
                       hum_in_loc = rep(c(hum_in1, hum_in2, hum_in3), 2))
  out_data = rbind(out_data[1, ], out_data[1, ], out_data[1, ],
                   out_data[2, ], out_data[2, ], out_data[2, ])
  data = cbind(in_data, out_data) %>% mutate(dp_loc = dpF(hum_in_loc, tem_in_loc),
                                             dp_out = dpF(hum_out_loc, tem_out_loc),
                                             dp129 = dpF(humid129, temp129),
                                             dp616 = dpF(humid616, temp616),
                                             fore_dp = dpF(fore_humid, fore_temp_3hour)) %>% 
    select("tem_in_loc", "hum_in_loc", "tem_coil_loc", "tem_out_loc", "hum_out_loc", "temp129", "rain_cum129", 
           "wind_dir129", "wind_speed129", "spot_press129","sea_press129","humid129","solar_rad129",
           "solar_amount129", "temp616", "rain616", "rain_status616","wind_dir616", "wind_speed616", "humid616", 
           "temp637","rain637", "rain_status637", "wind_dir637", "wind_speed637", "spot_press637", "sea_press637", 
           "fore_humid","fore_rain_prob","fore_sky_status", "fore_temp_3hour", "fore_wind_dir", "fore_wind_speed", 
           "dp_loc", "dp_out","dp616", "dp129", "fore_dp", "month", "day", "hour","min","season", "plant", 
           "location", "time")
  return(data)
}

# colorF ------------------------------------------------------------------


colorF <- function(prob, time = 24){
  if(time == 24){
    if(prob <= 1 && prob > 0.3){
      return("red")
    }else if(prob <= 0.3 && prob > 0.06){
      return("yellow")
    }else if(prob <= 0.06 && prob > 0.007){
      return("aqua")
    }else if(prob <= 0.007 && prob > 0.00008){
      return("blue")
    }else if(prob <= 0.00008 && prob > 0.000006){
      return("light-blue")
    }else{return("green")}
  }else{
    if(prob <= 1 & prob > 0.55){
      return("red")
    }else if(prob <= 0.3 && prob > 0.08){
      return("yellow")
    }else if(prob <= 0.06 && prob > 0.008){
      return("aqua")
    }else if(prob <= 0.007 && prob > 0.00005){
      return("blue")
    }else if(prob <= 0.00008 && prob > 0.000005){
      return("light-blue")
    }else{return("green")}
  }
}


# predict -----------------------------------------------------------------
predictF <- function(new_data){
  model = lgb$Booster(model_file = 'lgb.txt')
  new_data['prob_cond_loc'] = model$predict(as.matrix(new_data))
  
  data_24 = new_data %>% filter(time == 24)
  data_48 = new_data %>% filter(time == 48)
  data_24['cond_loc'] = ifelse(data_24$prob_cond_loc > 0.007, 1, 0) 
  data_48['cond_loc'] = ifelse(data_48$prob_cond_loc > 0.008, 1, 0)
  
  data_24$color = 0
  data_48$color = 0
  
  for(i in 1:3){
    data_24$color[i] = colorF(data_24$prob_cond_loc[i])
    data_48$color[i] = colorF(data_48$prob_cond_loc[i], time = 48)
  }
  
  data_24['icon'] = ifelse(data_24$cond_loc == 0, "smile", "frown")
  data_48['icon'] = ifelse(data_48$cond_loc == 0, "smile", "frown")
  
  return(rbind(data_24, data_48))
}


# crawling_129 ------------------------------------------------------------
# aws.date<- function(year, month, day, hour, min){
#   dt<- paste(year,".",month,".",day," ",hour,":",min, sep="")
#   time<- as.character(ymd_hm(dt)-min(240))
#   
#   aws<- paste(substr(time, 1, 4),".",substr(time, 6, 7),".",substr(time, 9, 16), sep = "")
#   return(aws)
# }
# 
# crawling<- function(year, month, day, hour, min){
#   
#   # output : 외부데이터 담은 data.frame 24, 48 2rows
#   remDr<-remoteDriver(remoteServerAddr="localhost", port=4445L, browserName="chrome")
#   remDr$open()
#   Sys.sleep(1)
#   remDr$navigate("https://www.weather.go.kr/weather/observation/aws_table_popup.jsp")
#   Sys.sleep(3)
#   
#   
#   # 서산 클릭
#   remDr$executeScript('parent.parent.menu.stn_select(129)')
#   Sys.sleep(3)
#   
#   cal<- remDr$findElement(using="id", value="datecal2")
#   cal$clearElement()
#   Sys.sleep(3)
#   cal$sendKeysToElement(list(aws.date(year, month, day, hour, min)))
#   Sys.sleep(1)
#   remDr$findElement(using='xpath', value='//*[@id="ui-datepicker-div"]/div[3]/button[2]')$clickElement()
#   Sys.sleep(1)
#   remDr$findElement(using='xpath', value='//*[@id="frm"]/table/tbody/tr/td[2]/input')$clickElement()
#   Sys.sleep(1)
#   
#   # frame부터 table로 이동
#   frames = remDr$findElement(using = "id", value = 'disp')
#   remDr$switchToFrame(frames)
#   
#   frames2 = remDr$findElement(using = "name", value = 'body')
#   remDr$switchToFrame(frames2)
#   
#   page<- remDr$getPageSource()[[1]]
#   webpage<- xml2::read_html(page)
#   
#   # table 값 뽑기
#   table<- list(NULL)
#   for(i in 3:17){
#     table[[i-2]]<-rvest::html_nodes(x=webpage, xpath=paste('/html/body/table/tbody/tr/td/table/tbody/tr[2]/td[',i,']', sep="")) %>% html_text()
#   }
#   result<- unlist(table)
#   
#   return(result)
# }



# main --------------------------------------------------------------------
mainF <- function(plant, tem_out, hum_out, tem_in1, tem_coil1, hum_in1,
                  tem_in2, tem_coil2, hum_in2, tem_in3, tem_coil3, hum_in3){
  # get datetime
  dt = getTime(); year <- dt[[1]]; month = dt[[2]]; day = dt[[3]]; hour = dt[[4]]; min = dt[[5]]; season = dt[[6]]
  
  #####
  # get out_data
  # result <- crawling(year, month, day, hour, min)
  # result2 <- as.numeric(result[c(7, 8, 10, 14, 15)])
  # #####
  #out_data <- read_csv('out_data.csv')
  out_data_final = read_csv("final_outdata.csv")
  out_data_final <- out_data_final %>% select(-spot_press616 ,-sea_press616)
  a <- ymd_hms(as.character(dt[[7]]-years(1)))
  
  
  b <- ymd_hms(as.character(out_data_final$date))
  
  t24 <- out_data_final[which.min(abs(a-b)),] %>% select(-date)
  t48 <- out_data_final[which.min(abs(a-b))+1,] %>% select(-date)
  out_data <- rbind(t24, t48)
  
  ##############
  
  # make new data frame
  new_data = newData(plant, tem_out, hum_out, tem_in1, tem_coil1, hum_in1,tem_in2, tem_coil2, hum_in2, tem_in3,
                     tem_coil3, hum_in3,month, day, hour, min, season,out_data)
  
  # predict
  final_data = predictF(new_data) %>% select(cond_loc, prob_cond_loc, color, icon)
  final_data["message"] = ifelse(final_data$cond_loc == 1, "발생", "미발생")
  
  return(final_data)
}


library(shinydashboard)
library(shiny)
library(dashboardthemes)


# ui ----------------------------------------------------------------------
# legend는 일단 나중에 고민해보기 *** 이건 꼭 넣어야함 <- prob 기준 3개 정하기!!
# 확률 계산하기 button이 필요함
# 시간 변수 따기
# isolate
ui <- dashboardPage(
  dashboardHeader(
    title = strong("결로 예측 경보 시스템"),
    titleWidth = 400),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    tags$head(tags$style(HTML('
                              .content-wrapper {background-color:white;}
                              .logo {
                              background-color: #184c72 !important;
                              }
                              .navbar {
                              background-color: #FFFFFF !important;
                              }
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-image:url("https://ifh.cc/g/1CpVFr.png");
                              background-position-x: 99%;
                              background-size: 200px 50px;
                              background-repeat: no-repeat;
                              }'
    ))),
    tags$style(
      type = 'text/css', 
      ".bg-red { background-color: #F46D43 !important; }",
      ".bg-yellow { background-color: #FDAE61 !important;  }",
      ".bg-aqua { background-color: #FEE090 !important;}",
      ".bg-blue { background-color: #E0F3F8 !important;  }",
      ".bg-light-blue { background-color: #ABD9E9 !important;  }",
      ".bg-green { background-color: #74ADD1 !important; }"
    ),
    
    tags$style(HTML("
                    .box.box-solid.box-primary>.box-header {
                    color:#FFFFFF;
                    background:#184c72;
                    text-align: center
                    }
                    
                    .box.box-solid.box-primary{
                    border-bottom-color:#184c72;
                    border-left-color:#184c72;
                    border-right-color:#184c72;
                    border-top-color:#184c72;
                    }
                    
                    ")),
    
    
    fluidRow(
      
      box(title = span("공장 정보", style ="color: white; font-weight: bold"), solidHeader = TRUE, width = 3, status = "primary",
          textInput("plant", "공장명", 1),
          textInput("tem_out", "공장 외부 대기 온도", 27.5),
          textInput("hum_out", "공장 외부 상대 습도", 67.9)),
      
      box(title = span("Location1", style ="color: white; font-weight: bold"), solidHeader = TRUE, width = 3, status = "primary",
          textInput("tem_in1", "Location 1의 대기 온도", 30.5),
          textInput("tem_coil1", "Location 1의 코일 표면 온도", 29.4),
          textInput("hum_in1", "Location 1의 상대 습도", 60.1)),
      
      box(title = span("Location2", style ="color: white; font-weight: bold"), solidHeader = TRUE, width = 3, status = "primary",
          textInput("tem_in2", "Location 2의 대기 온도", 29.8),
          textInput("tem_coil2", "Location 2의 코일 표면 온도", 28.9),
          textInput("hum_in2", "Location 2의 상대 습도", 61.9)),
      
      box(title = span("Location3", style ="color: white; font-weight: bold"), solidHeader = TRUE, width = 3, status = "primary",
          textInput("tem_in3", "Location 3의 대기 온도", 28.9),
          textInput("tem_coil3", "Location 3의 코일 표면 온도", 28.5),
          textInput("hum_in3", "Location 3의 상대 습도", 65.1))
      
    ),br(),
    fluidRow(
      column(6, align = "center", offset = 3,
             actionButton("go", label = strong("실행")))
      
    ),
    br(),
    
    fluidRow(
      uiOutput('result')
    )
    ))




server <- function(input, output) {
  
  # result
  observeEvent(input$go,{
    result = mainF(as.numeric(isolate({input$plant})), as.numeric(isolate({input$tem_out})), as.numeric(isolate({input$hum_out})), as.numeric(isolate({input$tem_in1})), 
                   as.numeric(isolate({input$tem_coil1})), as.numeric(isolate({input$hum_in1})),as.numeric(isolate({input$tem_in2})), 
                   as.numeric(isolate({input$tem_coil2})), as.numeric(isolate({input$hum_in2})), as.numeric(isolate({input$tem_in3})), 
                   as.numeric(isolate({input$tem_coil3})), as.numeric(isolate({input$hum_in3})))
    output$result <- renderUI(mainPanel(width = 12, 
                                        tabsetPanel(
                               
                                         tabPanel(strong("24hr"),br(),fluidRow(
                                           
                                           # value box
                                           valueBox(width = 4, value = span(result$message[1], style = 'color:black'), subtitle = span("Location1", style = 'color:black'),
                                                    icon(result$icon[1], lib = "font-awesome"), color = result$color[1]),
                                           
                                           valueBox(width = 4, value = span(result$message[2], style = 'color:black'), subtitle = span("Location2", style = 'color:black'),
                                                    icon(result$icon[2], lib = "font-awesome"), color =result$color[2]),
                                           
                                           valueBox(width = 4, value = span(result$message[3], style = 'color:black'), subtitle = span("Location3", style = 'color:black'),
                                                    icon(result$icon[3], lib = "font-awesome"), color = result$color[3])
                                           
                                         )),
                                         tabPanel(strong("48hr"),br(),fluidRow(
                                           # value box
                                           valueBox(width = 4, value = span(result$message[4], style = 'color:black'), subtitle = span("Location1", style = 'color:black'),
                                                    icon(result$icon[4], lib = "font-awesome"), color = result$color[4]),
                                           
                                           valueBox(width = 4, value = span(result$message[5], style = 'color:black'), subtitle = span("Location2", style = 'color:black'),
                                                    icon(result$icon[5], lib = "font-awesome"), color =result$color[5]),
                                           
                                           valueBox(width = 4, value = span(result$message[6], style = 'color:black'), subtitle = span("Location3", style = 'color:black'),
                                                    icon(result$icon[6], lib = "font-awesome"), color = result$color[6])
                                           
                                         ))), br(), div(img(src = 'https://ifh.cc/g/Q9tv7e.png'), style="text-align: center;")
                                         
                                         
                                  
    ))})
  
}

shinyApp(ui, server)



