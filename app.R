library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)


flagIcon <- makeIcon(
  iconUrl = case_when(
  gis_universities$code == "FR" ~ "Country_flags/法国.png",
  gis_universities$code == "AU" ~ "Country_flags/澳大利亚.png",
  gis_universities$code == "DK" ~ "Country_flags/丹麦.png",
  gis_universities$code == "CH" ~ "Country_flags/瑞士.png",
  gis_universities$code == "SE" ~ "Country_flags/瑞典.png",
  gis_universities$code == "DE" ~ "Country_flags/德国.png",
  gis_universities$code == "SK" ~ "Country_flags/韩国.png",
  gis_universities$code == "BE" ~ "Country_flags/比利时.png",
  gis_universities$code == "EN" ~ "Country_flags/英格兰.png",
  gis_universities$code == "ST" ~ "Country_flags/苏格兰.png",
  gis_universities$code == "IT" ~ "Country_flags/意大利.png",
  gis_universities$code == "US" ~ "Country_flags/美国.png",
  gis_universities$code == "FI" ~ "Country_flags/芬兰.png",
  gis_universities$code == "NZ" ~ "Country_flags/新西兰.png",
  gis_universities$code == "HK" ~ "Country_flags/中国香港.png",
  gis_universities$code == "HU" ~ "Country_flags/匈牙利.png",
  gis_universities$code == "CZ" ~ "Country_flags/捷克.png",
  gis_universities$code == "AT" ~ "Country_flags/奥地利.png",
  gis_universities$code == "NO" ~ "Country_flags/挪威.png",
  gis_universities$code == "IR" ~ "Country_flags/爱尔兰.png",
  gis_universities$code == "NE" ~ "Country_flags/荷兰.png",
  gis_universities$code == "SG" ~ "Country_flags/新加坡.png",
  gis_universities$code == "CA" ~ "Country_flags/加拿大.png"),
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
        filter(gis_universities, country == input$countries)
      }
    })
    
    filteredIcon <- reactive({
      if (input$countries == "全部国家或地区") {
        flagIcon
      } else {
        flagIcon$iconUrl <- rep(paste0("Country_flags/", str_replace_all(input$countries, " ", "_"), ".png"), 23)
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
  