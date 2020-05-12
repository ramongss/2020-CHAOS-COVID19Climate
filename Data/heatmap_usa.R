library(extrafont)
library(ggplot2)
library(Cairo)
library(tidyverse)
library(sf)
library(dplyr)
library(maps)


covid_max <- cbind(
  covid_max,
  Latitude = c(36.778259, 40, 42.407211, 39.833851,	43),
  Longitude = c(-119.417931, -89, -71.382439, -74.871826, -75)
)

covid_max$date <- NULL

x$region <- tolower(x$state)

x$state <- c('CA', 'IL', 'MA', 'NJ', 'NY')

states <- map_data("state")
map.df <- merge(states,x, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]

x$Longitude[3] <- -69

plot <- ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=confirmed))+
  geom_path()+ 
  geom_text(data=x, aes(x=Longitude,y=Latitude, group=NA, label=state), 
            size=5, vjust=0.5, hjust=0.5, family = "CM Roman")+
  theme(
    panel.grid = element_line(colour = "transparent"),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "CM Roman")
  ) +
  scale_fill_gradientn(colours = heat.colors(3, rev = TRUE), na.value = "white",
                       "Cumulative\nconfirmed cases") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm")) +
  coord_map()

plot %>% 
  ggsave(
    filename = "heatmap_usa.pdf",
    device = 'pdf',
    width = 9,
    height = 4.5,
    units = "in",
    dpi = 1200
  ) 
