library(extrafont)
# windowsFonts(Times=windowsFont("TT Times New Roman"))
library(ggplot2)
library(Cairo)

library(tidyverse)
library(sf)
library(dplyr)
library(brazilmaps)

# load data
covid <- readxl::read_xlsx("Covid_2004.xlsx") %>% 
  data.frame()

# choose columns
covid <- covid[,c("date", "state", "confirmed", "deaths")]

# split into list
covid_list <- covid %>% 
  split(covid$state)

# take the lastest results
covid_max <- covid_list %>% 
  sapply(function(x){tail(x,1)}) %>% 
  t() %>% 
  data.frame(); rownames(covid_max) <- NULL

# drop date column
covid_max <- covid_max[,-1]

# IBGE uf code
uf_code <- data.frame(
  code = c(c(11:17), c(21:29), c(31,32,33,35), c(41:43), c(50:53)),
  state = c("RO","AC","AM","RR","PA","AP","TO",
            "MA","PI","CE","RN","PB","PE","AL","SE","BA",
            "MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF")
)

# mutate the columns to join
covid_max <- covid_max %>% 
  mutate(state = as.character(state),
         confirmed = as.numeric(confirmed),
         deaths = as.numeric(deaths))

uf_code <- uf_code %>% 
  mutate(state = as.character(state))

# join the rows and fill the unmatched rows with NA
join <- covid_max %>% 
  full_join(uf_code)

# Matching the data with the map
uf_map <- get_brmap("State") %>% 
  inner_join(join, c("State" = "code"))

# plot the confirmed cases
confirmed_plot <- uf_map %>%
  ggplot() +
  geom_sf(aes(fill = confirmed), color = "black") +
  theme(
    panel.grid = element_line(colour = "transparent"),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "Times New Roman")
    ) +
  scale_fill_gradientn(colours = heat.colors(3, rev = TRUE), na.value = "white",
                       "Cumulative\nconfirmed cases") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm")) +
  annotate("text", 
           x = c(-65,-42,-39.5,-45,-52,-40,-34,-53,-50,-49),
           y = c(-4.5,-12,-4.5,-18,-24.5,-22,-5.5,-30,-27,-22),
           label = names(covid_list))

confirmed_plot %>% 
  ggsave(
    filename = "confirmed_map_plot.eps",
    device = cairo_ps,
    width = 6,
    height = 5,
    units = "in",
    dpi = 300
  ) 

# # plot the deaths cases
# deaths_plot <- uf_map %>% 
#   ggplot() +
#   geom_sf(aes(fill = deaths), color = "black") +
#   theme(
#     panel.grid = element_line(colour = "transparent"),
#     panel.background = element_blank(),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     text = element_text(family = "Times New Roman")
#     ) +
#   scale_fill_gradientn(colours = heat.colors(3, rev = TRUE), na.value = "white",
#                        "Deaths")

