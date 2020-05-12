# Set environment ----
rm(list = ls())
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# save several directories
BaseDir       <- getwd()
CodesDir      <- paste(BaseDir, "Codes", sep="/")
FiguresDir    <- paste(BaseDir, "Figures", sep="/")
ResultsDir    <- paste(BaseDir, "Results", sep="/")
DataDir       <- paste(BaseDir, "Data",sep="/")

# load Packages
setwd(CodesDir)
source("checkpackages.R")

packages<-c('vmd','dplyr','tidyverse','magrittr', 'caret', 'reshape2',
            'TTR', 'forecast', 'Metrics', "iterators", "doParallel",
            'e1071', "cowplot", "elmNNRcpp", "tcltk", "foreach",
            "iterators","doParallel","lmtest","wmtsa","magrittr")

sapply(packages,packs)

rm(packages)

library(extrafont)
# windowsFonts(Times = windowsFont("TT Times New Roman"))
library(ggplot2)
library(Cairo)

## Cores Cluster
# ncl <- detectCores();ncl
# cl  <- makeCluster(ncl-1);registerDoParallel(cl)
# stopImplicitCluster() # Stop

# Data treatment ----
# load data
setwd(DataDir)
covid <- read.csv('caso_full.csv')

# filter data for states
covid <- covid %>% 
  filter(place_type == 'state') %>%
  filter(state %in% c('AM','CE', 'PE', 'RJ', 'SP'))

# Separate the data
covid <- data.frame(
  date = covid$date %>% unlist %>% as.Date,
  state = covid$state %>% unlist,
  confirmed = covid$last_available_confirmed %>% unlist,
  deaths = covid$last_available_deaths %>% unlist,
  new_confirmed = covid$new_confirmed %>% unlist,
  new_deaths = covid$new_deaths %>% unlist
)

# Order by state and date
covid <- covid[order(covid$state, covid$date),]; rownames(covid) <- NULL

# load climate brazil
climate <- read.csv('bra_climate.csv')
climate$data <- climate$data %>% as.Date.character() # formate date

# join covid and climate
bra_data <- covid %>% 
  inner_join(y = climate, by = c('date' = 'data',
                                 'state' = 'code_estado'))

# remove covid and climate objects
rm(covid, climate)

# drop columns
bra_data <- bra_data[,-c(4,5,6,7,8,11,12,13)]

# rename last column
bra_data <- bra_data %>% 
  rename(precipitation = precipitacao)

# save data into csv
# write.csv(bra_data, 'bra_covid_climate.csv', row.names = FALSE)

# # plot new_confirmed cases by state
# bra_data %>% 
#   ggplot(aes(x = date)) +
#   geom_line(aes(y = new_confirmed)) +
#   facet_wrap(~state, scales = "free") +
#   scale_x_date(date_labels = "%d/%m")

# states by list
bra_data_list <- bra_data %>%
  split(bra_data$state)

# load U.S. data
usa_data <- read.csv('usa_covid_climate.csv')

# drop deaths column
usa_data$deaths <- NULL

# split into list by state
usa_data_list <- usa_data %>% 
  split(usa_data$state)


# Run the training ----

# load functions
setwd(CodesDir)
source('vmd_pred.R')
source('single_pred.R')

# vmd results for brazil
for (state in seq(bra_data_list)) {
  bra_data <- bra_data_list[[state]]
  bra_vmd_results <- vmd_pred(bra_data)
  setwd(ResultsDir)
  save(bra_vmd_results, file = paste0('bra_vmd_',
                                  bra_data_list[[state]]$state[1],
                                  '.RData'))
}

# vmd results for usa
for (state in seq(usa_data_list)) {
  usa_data <- usa_data_list[[state]]
  usa_vmd_results <- vmd_pred(usa_data)
  setwd(ResultsDir)
  save(usa_vmd_results, file = paste0('usa_vmd_',
                                      usa_data_list[[state]]$state[1],
                                      '.RData'))
}

# single models results for brazil
for (state in seq(bra_data_list)) {
  bra_data <- bra_data_list[[state]] %>% data.frame()
  bra_single_results <- single_pred(bra_data)
  setwd(ResultsDir)
  save(bra_single_results, file = paste0('bra_single_',
                                         bra_data_list[[state]]$state[1],
                                         '.RData'))
}

# single models results for usa
for (state in seq(usa_data_list)) {
  usa_data <- usa_data_list[[state]]
  usa_single_results <- single_pred(usa_data)
  setwd(ResultsDir)
  save(usa_single_results, file = paste0('usa_single_',
                                         usa_data_list[[state]]$state[1],
                                         '.RData'))
}