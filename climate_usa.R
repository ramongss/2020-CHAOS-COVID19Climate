rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Save several directories
BaseDir       <- getwd()
CodesDir      <- paste(BaseDir, "Codes", sep="/")
FiguresDir    <- paste(BaseDir, "Figures", sep="/")
ResultsDir    <- paste(BaseDir, "Results", sep="/")
DataDir       <- paste(BaseDir, "Data",sep="/")

library(httr)
library(magrittr)
library(jsonlite)
library(tidyverse)
library(dplyr)
# remotes::install_github("ropensci/rnoaa")
library(rnoaa)

apiKey <- "QaygEZTBaXzInAtwrXMupgxROuKDWKiL"

id_states <-  c("FIPS:01", "FIPS:02", "FIPS:04", "FIPS:05", "FIPS:06", "FIPS:08",
                "FIPS:09", "FIPS:10", "FIPS:11", "FIPS:12", "FIPS:13", "FIPS:15",
                "FIPS:16", "FIPS:17", "FIPS:18", "FIPS:19", "FIPS:20", "FIPS:21",
                "FIPS:22", "FIPS:23", "FIPS:24", "FIPS:25", "FIPS:26", "FIPS:27",
                "FIPS:28", "FIPS:29", "FIPS:30", "FIPS:31", "FIPS:32", "FIPS:33",
                "FIPS:34", "FIPS:35", "FIPS:36", "FIPS:37", "FIPS:38", "FIPS:39",
                "FIPS:40", "FIPS:41", "FIPS:42", "FIPS:44", "FIPS:45", "FIPS:46",
                "FIPS:47", "FIPS:48", "FIPS:49", "FIPS:50", "FIPS:51", "FIPS:53",
                "FIPS:54", "FIPS:55", "FIPS:56")

name_states <- c("Alabama"              ,"Alaska"
                 ,"Arizona"              ,"Arkansas"            
                 ,"California"           ,"Colorado"            
                 ,"Connecticut"          ,"Delaware"            
                 ,"District of Columbia" ,"Florida"             
                 ,"Georgia"              ,"Hawaii"              
                 ,"Idaho"                ,"Illinois"            
                 ,"Indiana"              ,"Iowa"              
                 ,"Kansas"               ,"Kentucky"            
                 ,"Louisiana"            ,"Maine"               
                 ,"Maryland"             ,"Massachusetts"       
                 ,"Michigan"             ,"Minnesota"           
                 ,"Mississippi"          ,"Missouri"            
                 ,"Montana"              ,"Nebraska"            
                 ,"Nevada"               ,"New Hampshire"       
                 ,"New Jersey"           ,"New Mexico"          
                 ,"New York"             ,"North Carolina"      
                 ,"North Dakota"         ,"Ohio"                
                 ,"Oklahoma"             ,"Oregon"              
                 ,"Pennsylvania"         ,"Rhode Island"        
                 ,"South Carolina"       ,"South Dakota"        
                 ,"Tennessee"            ,"Texas"               
                 ,"Utah"                 ,"Vermont"             
                 ,"Virginia"             ,"Washington"          
                 ,"West Virginia"        ,"Wisconsin"           
                 ,"Wyoming"
)

days <- seq(as.Date("2020-01-22"), as.Date("2020-04-28"), by = "days")

states <- data.frame(id_states, name_states)

selected_states <- states %>% 
  filter(name_states %in% c("New York", "New Jersey", "Massachusetts",
                            "Illinois", "Pennsylvania", "California"))

setwd(DataDir)

us_climate <- data.frame()

k <- 1

for (state in 1:length(selected_states$name_states)) {
  climate <- data.frame()
  for (day in 1:length(days)) {
    dados <- ncdc(datasetid  = 'GHCND',
                  startdate  = days[day],
                  enddate    = days[day],
                  locationid = selected_states$id_states[state],
                  limit      = 1000,
                  token      = apiKey)$data %>%
      data.frame
    
    dados <- dados %>%
      group_by(datatype) %>%
      summarise_at(vars(value), funs(mean)) %>%
      filter(datatype %in% c("PRCP","TMAX","TMIN")) %>%
      t() %>%
      data.frame()

    dados <- dados[-1,]; rownames(dados) <- NULL
    dados <- cbind(date = days[day], state = selected_states$name_states[state], dados)
    colnames(dados) <- c("date", "state", "prec", "tmax", "tmin")
    climate <- rbind(climate, dados)
    cat('\n',state,'. State: ', selected_states$name_states[state],
        '\tDay: ', as.character(days[day]),
        "\tRound #", k, sep = "")
    k <- k + 1
    Sys.sleep(5)
  }
  write.csv(climate, paste('climate_', selected_states$name_states[state],'_',
                              format.Date(Sys.time(), "%Y-%m-%d"),
                              '.csv', sep = ''), row.names = FALSE)
  cat("\n\nArquivo: climate_", 
      selected_states$name_states[state], "_",
      as.character(format.Date(Sys.time(), "%Y-%m-%d")),
      ".csv \tSALVO!\n", sep = "")
}

name_states <- c("New York", "New Jersey", "Massachusetts",
                 "Illinois", "Pennsylvania", "California")

usa_climate <- data.frame()

for (name in (name_states)) {
  dados <- readr::read_csv(paste('climate_',
                                 name,
                                 '_2020-04-30.csv',
                                 sep = ''))
  usa_climate <- rbind(usa_climate, dados)
  rm(dados)
}

usa_climate$tmax <- usa_climate$tmax/10
usa_climate$tmin <- usa_climate$tmin/10

usa_climate <- usa_climate[order(usa_climate$state),]

usa_covid <- readr::read_csv('usa_covid_2020-04-29.csv')

usa_data <- usa_covid %>% 
  right_join(usa_climate, c("date", "state"))

write.csv(usa_data, "usa_covid_climate.csv", row.names = FALSE)

