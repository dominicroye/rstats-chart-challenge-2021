library(shiny)
library(leaflet)
library(htmltools)
library(leaflet.extras)

library(tidyverse)
library(lubridate)

df_cv19 <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")


df<-df_cv19 %>%
  mutate(lon=long, cases=totale_casi,
         region=denominazione_regione,
         date=format(as.Date(data,"%Y-%m-%d")),
         date=as.Date(date)) %>%
  group_by(date)%>%
  summarize(avg_cases=mean(cases))%>%
  mutate(id=seq(1,length(avg_cases),by=1))%>%
  ungroup()
  
  
  
  select(date,region,lat,lon,cases)%>%
  arrange(date)%>%
  group_by(date) %>%
  summarize(mean_cases=mean(cases),lat,lon)%>%
  mutate(day=seq(1,length(date)))
 

ui <- bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                    
                    leafletOutput("map", width = "100%", height = "100%"),
                    
                    absolutePanel(top = 1, right = 10, draggable = TRUE,
                                  sliderInput("time", "Tracking Covid19 Italy (day)",
                                              min(df$day), max(df$day), value = min(df$day), sep = "")))


server <- function(input, output){
  
  output$map <- renderLeaflet({
    
    leaflet(data = df %>% 
              filter(day <= input$time[])) %>% 
      
      addTiles() %>% 
      setView(lng = 12.5, lat = 41.9, zoom = 6) %>% #lazio
      
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addCircleMarkers(~lon, ~lat, radius = 6, fillOpacity = .5, color = 'purple') %>% 
      
      addEasyButton(easyButton(
        icon = "fa-globe",
        onClick = JS("function(btn, map){ map.setZoom(3);}"))) %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Locate Me",
        onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
     
      addFullscreenControl() %>%
      addScaleBar(position = "bottomleft")
  })
}


shinyApp(ui = ui, server = server)










