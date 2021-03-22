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
            "jsonlite","magrittr")

sapply(packages,packs)

library(extrafont)
library(ggplot2)
library(Cairo)
#remotes::install_github("joachim-gassen/tidycovid19")
library(tidycovid19)
library(tidyverse)
library(sf)
library(dplyr)

# #Checa quantos núcleos existem
# ncl<-detectCores();ncl
# 
# #Registra os clusters a serem utilizados
# cl <- makeCluster(ncl-1);registerDoParallel(cl)

#########################################################################

# load usa data
usa_conf  <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
usa_death <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

# drop columns
usa_conf  <- usa_conf[,-c(1:6,8:11)]
usa_death <- usa_death[,-c(1:6,8:12)] 

# order by state
usa_conf <- usa_conf[order(usa_conf$Province_State),]
usa_death <- usa_death[order(usa_death$Province_State),]

# sum all data by state
usa_conf <- usa_conf %>% 
  group_by(Province_State) %>% 
  summarise_all(funs(sum))

usa_death <- usa_death %>% 
  group_by(Province_State) %>% 
  summarise_all(funs(sum))

# transpose table
usa_conf <- usa_conf %>% t() %>% data.frame()
usa_death <- usa_death %>% t() %>% data.frame()

# rename columns and drop 1st line
colnames(usa_conf) <- usa_conf[1,]; colnames(usa_death) <- usa_death[1,]
usa_conf <- usa_conf[-1,]; usa_death <- usa_death[-1,] 

# rownames as 1st column
usa_conf <- cbind(date = rownames(usa_conf), usa_conf)
rownames(usa_conf) <- NULL # reset rownames

usa_death <- cbind(date = rownames(usa_death), usa_death)
rownames(usa_death) <- NULL # reset rownames


# format date column
usa_conf$date <- usa_conf$date %>% as.Date(format = "%m/%d/%y")
usa_death$date <- usa_death$date %>% as.Date(format = "%m/%d/%y")

# stacking the variables
usa_conf <- reshape2::melt(usa_conf, id.vars=1)
usa_death <- reshape2::melt(usa_death, id.vars=1)

# rename columns
colnames(usa_conf) <- c("date","state","confirmed")
colnames(usa_death) <- c("date","state","deaths")

# merge dataframes
usa_covid <- usa_conf %>% 
  full_join(usa_death, c("date", "state"))

# format numerical columns
usa_covid$confirmed <- usa_covid$confirmed %>% as.numeric()
usa_covid$deaths    <- usa_covid$deaths %>% as.numeric()

# plot by state
usa_covid %>%
  # filter(state == "Alabama") %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = confirmed)) +
  facet_wrap(~state, scales = "free") +
  scale_x_date(date_labels = "%d/%m")

# save usa_covid data
setwd(DataDir)
usa_covid %>% 
  write.csv(paste('usa_covid_',
                  format.Date(Sys.time(), "%Y-%m-%d"),'.csv', sep = ""),
            row.names = FALSE)
###########################################################################

setwd(DataDir)
usa_covid <- readr::read_csv('usa_covid_2020-04-29.csv')

usa_covid_list <- usa_covid %>% 
  split(usa_covid$state)

last_record <- data.frame()

for (ii in seq(usa_covid_list)) {
  last_record <- rbind(last_record,
                       tail(usa_covid_list[[ii]],1))
}

last_record[order(last_record$confirmed, decreasing = TRUE),]

