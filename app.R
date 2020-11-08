library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(readr)
gis_universities <- read_csv("gis_universities.csv")
gis_universities$popup <- ""

for(i in 1:nrow(gis_universities)){
  cn_name = toString(gis_universities$cn[i])
  en_name = toString(gis_universities$name[i])
  s_name = toString(gis_universities$eng[i])
  deg = toString(gis_universities$degrees[i])
  country = toString(gis_universities$country[i])
  link = toString(gis_universities$web[i])
  
  gis_universities$popup[i] = paste0("<center></br><h3>",cn_name,"</h3></center></br>",en_name," (",s_name,")</br>","<b>所在地</b>: ",country,"</br><b>开设学位</b>: ",deg,"</br><a href='",link,"' target='_blank'>项目网页...</a>")
}


flagIcon <- makeIcon(
  iconUrl = case_when(
  gis_universities$code == "FR" ~ "Country_flags/FR.png",
  gis_universities$code == "AU" ~ "Country_flags/AU.png",
  gis_universities$code == "DK" ~ "Country_flags/DK.png",
  gis_universities$code == "CH" ~ "Country_flags/CH.png",
  gis_universities$code == "SE" ~ "Country_flags/SE.png",
  gis_universities$code == "DE" ~ "Country_flags/DE.png",
  gis_universities$code == "SK" ~ "Country_flags/SK.png",
  gis_universities$code == "BE" ~ "Country_flags/BE.png",
  gis_universities$code == "EN" ~ "Country_flags/EN.png",
  gis_universities$code == "ST" ~ "Country_flags/ST.png",
  gis_universities$code == "IT" ~ "Country_flags/IT.png",
  gis_universities$code == "US" ~ "Country_flags/US.png",
  gis_universities$code == "FI" ~ "Country_flags/FI.png",
  gis_universities$code == "NZ" ~ "Country_flags/NZ.png",
  gis_universities$code == "HK" ~ "Country_flags/HK.png",
  gis_universities$code == "HU" ~ "Country_flags/HU.png",
  gis_universities$code == "CZ" ~ "Country_flags/CZ.PNG",
  gis_universities$code == "AT" ~ "Country_flags/AT.png",
  gis_universities$code == "NO" ~ "Country_flags/NO.png",
  gis_universities$code == "IR" ~ "Country_flags/IR.png",
  gis_universities$code == "NE" ~ "Country_flags/NE.png",
  gis_universities$code == "SG" ~ "Country_flags/SG.png",
  gis_universities$code == "CA" ~ "Country_flags/CA.png"),
iconWidth = 40, iconHeight = 40,
shadowWidth = 20, shadowHeight = 20
)

ui <- bootstrapPage(
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(top = 10, right = 10,
                pickerInput("countries", label = "选择国家或地区:",
                            choices = list("全部国家或地区", 
                                           `North America` = c("美国","加拿大"),
                                           `Europe` = c("德国","法国","英格兰","苏格兰","瑞士","瑞典","挪威","丹麦","芬兰","匈牙利","捷克","意大利","奥地利","爱尔兰","比利时","荷兰"),
                                           `Oceania` = c("澳大利亚","新西兰"),
                                           `Asia` = c("中国香港","韩国","新加坡")
                            )
                )
  )
)
  
  server <- function(input, output, session) {
    
    filteredData <- reactive({
      if (input$countries == "全部国家或地区") {
        gis_universities
      } else {
        if(input$countries == "法国"){
          filter(gis_universities, code == "FR")
        }else if(input$countries == "德国"){
          filter(gis_universities, code == "DE")
        }else if(input$countries == "英格兰"){
          filter(gis_universities, code == "EN")
        }else if(input$countries == "苏格兰"){
          filter(gis_universities, code == "ST")
        }else if(input$countries == "爱尔兰"){
          filter(gis_universities, code == "IR")
        }else if(input$countries == "意大利"){
          filter(gis_universities, code == "IT")
        }else if(input$countries == "瑞士"){
          filter(gis_universities, code == "CH")
        }else if(input$countries == "捷克"){
          filter(gis_universities, code == "CZ")
        }else if(input$countries == "瑞典"){
          filter(gis_universities, code == "SE")
        }else if(input$countries == "芬兰"){
          filter(gis_universities, code == "FI")
        }else if(input$countries == "丹麦"){
          filter(gis_universities, code == "DK")
        }else if(input$countries == "挪威"){
          filter(gis_universities, code == "NO")
        }else if(input$countries == "匈牙利"){
          filter(gis_universities, code == "HU")
        }else if(input$countries == "奥地利"){
          filter(gis_universities, code == "AT")
        }else if(input$countries == "荷兰"){
          filter(gis_universities, code == "NE")
        }else if(input$countries == "比利时"){
          filter(gis_universities, code == "BE")
        }else if(input$countries == "美国"){
          filter(gis_universities, code == "US")
        }else if(input$countries == "加拿大"){
          filter(gis_universities, code == "CA")
        }else if(input$countries == "中国香港"){
          filter(gis_universities, code == "HK")
        }else if(input$countries == "韩国"){
          filter(gis_universities, code == "SK")
        }else if(input$countries == "新加坡"){
          filter(gis_universities, code == "SG")
        }else if(input$countries == "澳大利亚"){
          filter(gis_universities, code == "AU")
        }else if(input$countries == "新西兰"){
          filter(gis_universities, code == "NZ")
        }
      }
    })
    
    filteredIcon <- reactive({
      if (input$countries == "全部国家或地区") {
        flagIcon
      } else {
        if(input$countries == "法国"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/FR.png"), 23)
        }else if(input$countries == "德国"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/DE.png"), 23)
        }else if(input$countries == "英格兰"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/EN.png"), 23)
        }else if(input$countries == "苏格兰"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/ST.png"), 23)
        }else if(input$countries == "爱尔兰"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/IR.png"), 23)
        }else if(input$countries == "瑞士"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/CH.png"), 23)
        }else if(input$countries == "瑞典"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/SE.png"), 23)
        }else if(input$countries == "奥地利"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/AT.png"), 23)
        }else if(input$countries == "匈牙利"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/HU.png"), 23)
        }else if(input$countries == "芬兰"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/FI.png"), 23)
        }else if(input$countries == "荷兰"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/NE.png"), 23)
        }else if(input$countries == "捷克"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/CZ.PNG"), 23)
        }else if(input$countries == "挪威"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/NO.png"), 23)
        }else if(input$countries == "丹麦"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/DK.png"), 23)
        }else if(input$countries == "比利时"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/BE.png"), 23)
        }else if(input$countries == "意大利"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/IT.png"), 23)
        }else if(input$countries == "美国"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/US.png"), 23)
        }else if(input$countries == "加拿大"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/CA.png"), 23)
        }else if(input$countries == "新加坡"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/SG.png"), 23)
        }else if(input$countries == "中国香港"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/HK.png"), 23)
        }else if(input$countries == "韩国"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/SK.png"), 23)
        }else if(input$countries == "澳大利亚"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/AU.png"), 23)
        }else if(input$countries == "新西兰"){
          flagIcon$iconUrl <- rep(paste0("Country_flags/NZ.png"), 23)
        }

      }
      flagIcon
    })
    
    output$map <- renderLeaflet({
      leaflet(filteredData()) %>%
        addTiles(urlTemplate = "http://www.google.cn/maps/vt?lyrs=s@189&gl=cn&x={x}&y={y}&z={z}",attribution = 'Google') %>%
        #addProviderTiles(providers$TianDiTu.Satellite.Map) %>%
        #addChinaTiles(providers$TianDiTu.Satellite.Map) %>%
        addMarkers(~lon, ~lat, 
                   icon = filteredIcon(), 
                   #label = ~Player, 
                   labelOptions = labelOptions(textsize = "12px"),
                   popup = ~popup)
    })
    
    observe({
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        addMarkers(~lon, ~lat, 
                   icon = filteredIcon(), 
                   #label = ~Player, 
                   labelOptions = labelOptions(textsize = "12px"),
                   popup = ~popup)
    })
  }
  
  shinyApp(ui, server)
  