
# inspired by: https://github.com/ChrisWoodsSays/TidyTuesday/tree/master/2021/2021-03-23



# Load packages
#install.packages("pacman")
pacman::p_load(ihme.covid,
               tidyverse, lubridate, stringi, 
               unvotes, wesanderson)



library(glue)
library(patchwork)
library(ggtext)
#library(showtext)
library(extrafont)
library(hrbrthemes)




setwd(here::here("GBD/IHME/IHME_forked/Covid19"))

ihme_covid<- read.csv("2021-01-15/reference_hospitalization_all_locs.csv",header=TRUE)


it_regions<-c("Abruzzo",
              "Valle d'Aosta",
              "Puglia",
              "Basilicata",
              "Calabria",
              "Campania",
              "Emilia-Romagna",
              "Friuli-Venezia Giulia",
              "Lazio",
              "Liguria",
              "Lombardia",
              "Molise",
              "Piemonte",
              "Sardegna",
              "Sicilia",
              "Marche",
              "Toscana",
              "Umbria",
              "Veneto",
              "Provincia autonoma di Bolzano",
              "Provincia autonoma di Trento")



ihme_covid_back_up<-ihme_covid

ihme_covid_ICU<-ihme_covid_back_up%>%
  filter(location_name==it_regions)%>%
  select(-V1,-location_id)


ihme_covid_ICU<-data.frame(ihme_covid_ICU)

icu_bed_var<-ihme_covid_ICU %>%
  select(location_name,date,ICUbed_mean:ICUbed_upper) %>%
  pivot_longer(ICUbed_mean:ICUbed_upper, 
               names_to = "comparison",
               values_to = "count") %>%
  mutate(date=as.Date(date,"%Y-%m-%d"),
         time=seq(1,length(date),1),
                year = lubridate::year(date),
                month = lubridate::month(date),
         week=week(date),
         proportion = 100 * count/sum(count))




##################################################################

startDate <- min(icu_bed_var$week)
endDate <- max(icu_bed_var$week)


scaleFactor = 3

innerLine <- 20 * scaleFactor

middleLine <- 30 * scaleFactor

outerLine <- 40 * scaleFactor

alpha = 1

# Get palette
Royal1 <- wesanderson::wes_palettes$Royal1


g <- ggplot() +
  
  geom_ribbon(data = icu_bed_var %>% filter(comparison == "ICUbed_mean"), 
              aes(x = week, ymin = middleLine - count, ymax = middleLine + count, 
                  fill = comparison), 
              alpha = alpha/1, stat="identity", colour = NA) +
  
  geom_ribbon(data = icu_bed_var %>% filter(comparison == "ICUbed_upper"), 
              aes(x = week, ymin = outerLine , ymax = outerLine + count, fill = comparison), 
              alpha = alpha, stat="identity", colour = NA) +
   
  geom_ribbon(data = icu_bed_var %>% filter(comparison == "ICUbed_lower"), 
              aes(x = week, ymin = innerLine, ymax = innerLine - count, fill = comparison), 
              alpha = alpha, stat="identity", colour = NA) +
  
  labs(title = "Covid19 Italy regions - ICU beds capacity",
       subtitle = "situation from Feb 2020 to March 2021 by week",
       caption = "27.3.2021  |  Visualisation by @fgazzelloni  |  Data:IHME") +

  theme_ft_rc() +
  coord_polar(direction=1) +
  
  scale_x_continuous(breaks=seq(startDate, endDate, by=round((endDate - startDate)/2)), 
                     expand=c(0,0), lim=c(startDate, endDate)) +
  
  scale_fill_manual(values = Darjeeling1) +
  
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        strip.text.x = element_text(hjust = 0.5,size=8),
        strip.text.y = element_text(hjust = 0.5, size = 5),
        panel.grid.minor.x = element_blank(),
        panel.spacing.x = unit(0, "lines"), # Remove horizontal spacing between facets
        panel.spacing.y = unit(0, "lines"), # Remove vertical pacing between facets
        plot.title.position = "plot",
        
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA),
        plot.margin = margin(5,5,5,5)
  ) +
  
  facet_wrap(~location_name,
             labeller = label_wrap_gen()) 
  



#######################################################################################


# Create non radial legend with a sample of data
dataLegend <- icu_bed_var  %>% filter(location_name == "Lazio" & year == "2020") 

dataLegend$comparison = factor(dataLegend$comparison, levels=c('ICUbed_upper','ICUbed_mean','ICUbed_lower'))

legend <- ggplot() +
  
  geom_ribbon(data = dataLegend %>% filter(comparison == "ICUbed_mean"), 
              aes(x = week, 
                  ymin = middleLine - count/2, ymax = middleLine + count/2, 
                  fill = comparison), alpha = alpha/1, stat="identity", colour = NA) +
  geom_ribbon(data = dataLegend %>% filter(comparison == "ICUbed_upper"),
              aes(x = week, 
                  ymin = 20, ymax = 20 + count, 
                  fill = comparison), alpha = alpha, stat="identity", colour = NA) +
  geom_ribbon(data = dataLegend %>% filter(comparison == "ICUbed_lower"),
              aes(x = week, 
                  ymin = 23 - count/3, ymax = 23, 
                  fill = comparison), alpha = alpha, stat="identity", colour = NA) +
  
  theme_void() +
  coord_polar(direction=1) +
  scale_fill_manual(values = Darjeeling1) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        
        panel.spacing.x = unit(1, "lines"), # Increase horizontal spacing between facets
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        strip.text.x = element_blank(),
        
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black", color = "black"),
        ) +
  facet_wrap(vars(comparison))




# Add facets, text and legend together and plot


plot <- cowplot::ggdraw(g) +
  
  cowplot::draw_plot(legend, 0.4,0, 0.25, .2, scale=1.1) +
  
  cowplot::draw_label("ICUbed_upper", x = 0.42, y = 0.15, colour = "#929299", size = 8, hjust = 0.5) +
  cowplot::draw_label("ICUbed_mean", x = 0.52, y = 0.15, colour = "#929299", size = 8, hjust = 0.5) +
  cowplot::draw_label("ICUbed_lower", x = 0.62, y = 0.15, colour = "#929299", size = 8, hjust = 0.5)


ggsave(here::here("GBD/IHME/IHME_forked", "covid19_icu.png"), 
       plot, device = "png", width = 29.65, height = 21, 
       units = "cm")















