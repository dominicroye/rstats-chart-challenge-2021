library(shiny)
library(leaflet)
library(htmltools)
library(leaflet.extras)

library(tidyverse)
library(lubridate)

library(shinyWidgets)

df_cv19 <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")


df_cv19_sub<-df_cv19 %>%
  mutate(lon=long, 
         cases=totale_casi,
         region=denominazione_regione,
         date=format(as.Date(data,"%Y-%m-%d")),
         date=as.Date(date)) %>%
  select(date,region,lat,lon,cases)%>%
  filter(!cases==0)

df_cv19_sub<-as.data.frame(df_cv19_sub)
  
mean_cases<-df_cv19%>%mutate(lon=long, 
                             cases=totale_casi,
                             region=denominazione_regione,
                             date=format(as.Date(data,"%Y-%m-%d")),
                             date=as.Date(date)) %>%
  group_by(date)%>%summarize(mean(cases))%>%
  mutate(day=seq(1,length(date),by=1))

mean_cases<-as.data.frame(mean_cases)


summary(df_cv19$deceduti)
summary(df$av_deaths)

df<-df_cv19%>%
  mutate(lon=long,
         cases=totale_casi,
         deaths=deceduti,
         CFR=deceduti/totale_casi,
         region=denominazione_regione,
         date=format(as.Date(data,"%Y-%m-%d")),
         date=as.Date(date)) %>%
  group_by(date)%>%
  summarize(av_cases=mean(cases),
            av_deaths=mean(deaths))%>%
  mutate(new_deaths=c(0,diff(av_deaths)),
         new_cases=c(0,diff(av_cases)),
         av_CFR=av_deaths/av_cases*100,
         day=seq(1,length(date),by=1),
         week=week(date))


write.csv(df,"mean_values.csv")

df<-full_join(df_cv19_sub,mean_cases,by.x=date)%>%
  filter(day<=11)


ui <- bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                    
                    leafletOutput("map", width = "100%", height = "100%"),
                    
                    absolutePanel(top = 1, right = 10, draggable = TRUE,
                                  chooseSliderSkin("Modern"),
                                  setSliderColor("DeepPink",1),
                                  sliderInput("time", "Tracking first 10 days of Covid19 Italy",
                                              min(df$day), max(df$day), value = min(df$day), sep = "")))


server <- function(input, output){
  
  output$map <- renderLeaflet({
    
    leaflet(data = df %>% 
              filter(day <= input$time[])) %>% 
      
      addTiles() %>% 
      
      setView(lng = 12.5, lat = 41.9, zoom = 6) %>% #lazio
      
      #addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      
      addProviderTiles(providers$Hydda.Base, group = "Hydda") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Carto") %>%
      addLayersControl(baseGroups = c("Hydda", "Carto"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      
      addCircleMarkers(~lon, ~lat, radius = 26, fillOpacity = .5, color = 'purple') %>%
      addMarkers(clusterOptions = markerClusterOptions())%>%
      addCircles(lng = ~lon, lat = ~lat, weight = 1,
                 radius = ~sqrt(cases) * 30, popup = ~region ) %>%
      
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










