rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Save several directories
BaseDir       <- getwd()
CodesDir      <- paste(BaseDir, "Codes", sep="/")
FiguresDir    <- paste(BaseDir, "Figures", sep="/")
ResultsDir    <- paste(BaseDir, "Results", sep="/")
DataDir       <- paste(BaseDir, "Data",sep="/")

#Load Packages
setwd(CodesDir)
source("checkpackages.R")

packages<-c("forecast","cowplot","Metrics","caret","elmNNRcpp","tcltk","TTR",
            "foreach","iterators","doParallel","lmtest","wmtsa","httr",
            "jsonlite","magrittr","xlsx")

sapply(packages,packs)

library(extrafont)
# windowsFonts(Times = windowsFont("TT Times New Roman"))
library(ggplot2)
library(Cairo)
library(magrittr)
library(tidyverse)
library(dplyr)

#################################################
# load data
setwd(DataDir)
covid <- read.csv('caso.csv')

# filter data for states
covid <- covid %>% 
  filter(place_type == 'state') %>%
  filter(state %in% c('AM','CE', 'PB', 'RJ', 'SP'))

# Separate the data
covid <- data.frame(
  date        = covid$date      %>% unlist %>% as.Date,
  state       = covid$state     %>% unlist,
  confirmed   = covid$confirmed %>% unlist,
  deaths      = covid$deaths    %>% unlist
)

# Order by state and date
covid <- covid[order(covid$state, covid$date),]; rownames(covid) <- NULL

# Plot confirmed cases by state
covid %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = confirmed)) +
  facet_wrap(~state, scales = "free") +
  scale_x_date(date_labels = "%d/%m")

# States by list
covid_list <- covid %>%
  split(covid$state)

# Insert incidence into list
for(i in 1:length(covid_list)) {
  covid_list[[i]]$confirmed_incidence <-rep(0,times=dim(covid_list[[i]])[1])
  covid_list[[i]]$deaths_incidence<-rep(0,times=dim(covid_list[[i]])[1])
  
  for(j in 1:dim(covid_list[[i]])[1]) {
    if(j==1) {
      covid_list[[i]][j,"confirmed_incidence"] <-covid_list[[i]][j,"confirmed"]
      covid_list[[i]][j,"deaths_incidence"]<-covid_list[[i]][j,"deaths"]
    } else {
      covid_list[[i]][j,"confirmed_incidence"] <-covid_list[[i]][j,"confirmed"]-covid_list[[i]][j-1,"confirmed"]
      covid_list[[i]][j,"deaths_incidence"]<-covid_list[[i]][j,"deaths"]-covid_list[[i]][j-1,"deaths"]
    }
  }
}

# unlist again
covid <- covid_list %>%
  map_df(~.)

# load climate brazil
climate <- read.csv('bra_climate.csv')
climate$data <- climate$data %>% as.Date.character() # formate date

# join covid and climate
bra_data <- covid %>% 
  inner_join(y = climate, by = c('date' = 'data',
                                 'state' = 'code_estado'))

# reorder columns
bra_data <- bra_data[,c(1,7,2,3,4,5,6,8:ncol(bra_data))]

# save data into csv
write.csv(bra_data, 'bra_covid_climate.csv', row.names = FALSE)
