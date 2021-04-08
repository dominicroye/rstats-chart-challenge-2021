
# libraries --------------------
library(tidytuesdayR)
library(tidyverse)


# load data and wrangling ----------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 15)

forest <- tuesdata$forest
forest_area <- tuesdata$forest_area
brazil_loss <- tuesdata$brazil_loss
soybean_use <- tuesdata$soybean_use
vegetable_oil <- tuesdata$vegetable_oil


svdf <- merge(soybean_use,vegetable_oil,na.rm=T)
mean(is.na(svdf))
svdf[is.na(svdf)]=0


Soybean <- svdf %>% filter(crop_oil=="Soybean")%>%
  group_by(year) %>% 
  summarize(avg_prod=mean(production))

pivot <- svdf %>%
  pivot_longer(cols=c(4,5),names_to="name",values_to="value")%>%
  filter(crop_oil=="Soybean")

tapply(svdf$production,svdf$crop_oil,summary)

# plotting --------------------------------------------

library(ggExtra)
library(xkcd)
library(ggstatsplot)
library(extrafont)

xrange <- range(pivot$production)
yrange <- range(pivot$value)

experiment_plot <- ggplot() +
  geom_point(data=pivot, aes(x = production, y = value,fill=name),
             size = 2, shape = 21, colour = "black" ) +
  xkcdaxis(xrange,yrange) + 
  scale_x_continuous(labels = scales::label_number_si()) + 
  scale_y_continuous(labels = scales::label_number_si()) + 
  annotate("text", x=19000000, y = 17478000,label = "Animal feed", family="xkcd" )+
  annotate("text", x=24000000, y = 17478000,label = "Human food", family="xkcd" )+
  labs(title="Experimenting production of Soybean",
       subtitle = "for Human food and Animal feed",
       caption = "Viz federica gazzelloni - Datasource: Our World in data - Day6") +
  geom_hline(yintercept = 6000000, colour = "black", linetype = "dashed", size = 0.2) + 
  geom_vline(xintercept = 32000000, colour = "black", linetype = "dashed", size = 0.2) + 
  annotate("rect", xmin = 32000000, xmax = 42845399, ymin = 6000000, ymax = 17478000, 
           alpha = .3) +
  annotate("text", x=35000000, y = 4000000,
           label = "A level of production of Soybean over 35M shows \na change in trend directions to a higher level \nof animal feed when compared with human food consumption.",
           family="xkcd" ) +
  theme_xkcd() + 
  theme(legend.position="top",
        legend.text = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        plot.subtitle = element_text(size=10),
        axis.text = element_text(family="Comic Sans MS"),
        axis.text.x = element_text(size=8,vjust = 0.2),
        axis.text.y = element_text(size=8,hjust = 0.8),
        axis.ticks = element_line())

# save the plot ----------------------------

ragg::agg_png(here::here("day6", "Experiment_day6.png"),
              res = 320, width = 14, height = 8, units = "in")
experiment_plot

dev.off()

# -----------------------