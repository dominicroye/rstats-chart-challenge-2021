

library(tidyverse)

df <- read.csv("mean_values.csv")
regions <- read.csv("Map_covid19_first_10_days_it.csv")

####################################################

(g <- ggplot(df, aes(x = new_deaths, y = new_deaths+rnorm(nrow(df), sd = 20))))

deaths <- g + 
  geom_point(color = "firebrick") + 
  geom_rug() + 
  labs(x = "Deaths", y = "noise",
       subtitle = "Historical pattern of daily deaths from Feb 2020 to present",
       caption = "Data:Civil Protection, Viz@fgazzelloni"
       ) +
  ggtitle("AVG Regions'Deaths due to Covid19 in Italy") +
  expand_limits(x = 0, y = 0) +
  coord_fixed(ratio = 1/4)+
  theme_void()+
  theme(
        plot.title = element_text(face = "bold",margin = margin(10, 0, 10, 0),size = 30,
                                  colour="dodgerblue"),
        plot.subtitle = element_text(face = "bold",size = 12,
                                     colour="dodgerblue"),
        plot.caption = element_text(face = "bold",hjust =0.5,vjust = 0),
        axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 1, size = 15),
       
        axis.title = element_text(size = 15, color = "firebrick",
                          face = "bold"),
        
        axis.text = element_text(color = "dodgerblue", size = 12,vjust = 2),
        axis.text.x = element_text(face = "bold",angle = 0, vjust = 0, hjust = 2, size = 12),
        axis.text.y = element_text(face = "bold"),
        
        axis.line = element_line(colour="dodgerblue"),
        plot.margin = margin(t = 10, r = 0, b = 10, l = 0, unit = "pt")
  )
        

###################################################################
library(leaflet)

img <- "https://www.r-project.org/logo/Rlogo.svg"

map <- leaflet(regions) %>% 
  addTiles() %>% 
  leafem::addLogo(img, url = "https://www.r-project.org/logo/")%>%
  addCircles(lng = ~lon, lat = ~lat, weight = 1,
             radius = ~sqrt(cases) * 30, popup = ~region ) %>%
  
  addCircleMarkers(~lon, ~lat, radius = 25, fillOpacity = .05, color = 'purple') %>%
  addMarkers(clusterOptions = markerClusterOptions()) %>%
  addProviderTiles(providers$Stamen.TonerBackground) 
  
   
##############################################################

library(ggimage)

map_img="map.png"
final <- ggbackground(deaths, map_img, alpha=.3)


ggsave(here::here("day3","Covid19_deaths.png"), final, device = "png", width = 29.65, height = 21, 
       units = "cm")

final
dev.off() 


  