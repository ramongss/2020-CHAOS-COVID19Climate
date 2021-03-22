rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Save several directories
BaseDir       <- getwd()
CodesDir      <- paste(BaseDir, "Codes", sep="/")
FiguresDir    <- paste(BaseDir, "Figures", sep="/")
ResultsDir    <- paste(BaseDir, "Results", sep="/")
DataDir       <- paste(BaseDir, "Data",sep="/")

library(magrittr)
library(tidyverse)
library(dplyr)

#################################################
setwd(DataDir)

# name of the states
name_states <- c("Amazonas",
                 "Ceara",
                 "Pernambuco",
                 "Rio",
                 "SaoPaulo")

code_states <- c('AM', 'CE', 'PE', 'RJ', 'SP')

# dataframe to save the data
bra_climate <- data.frame()

for (name in 1:length(name_states)) {
  # read csvs
  dados <- read.csv(paste('climate_',name_states[name],'_2020-04-30.csv',sep = ''), sep = ';')
  
  # keep usefull columns
  dados <- dados[,c(2,4:9,20)]
  
  # format value columns as numeric
  dados[,-1] <- dados[,-1] %>% sapply(as.numeric)
  
  # format date column as date
  if (name == 2 || name == 5 ) {
    dados$data <- dados$data %>% as.Date(format = "%d/%m/%Y")
  } else{
    dados$data <- dados$data %>% as.Date(format = "%Y/%m/%d")
  }

  # drop NA rows
  dados <- dados %>% drop_na(c(temp_inst, temp_max, temp_min, precipitacao))

  # grup the columns by date and summarise as mean
  dados <- dados %>%
    group_by(data) %>%
    summarise_all(funs(mean))

  # create state name column
  dados <- cbind(estado = rep(name_states[name]),
                 code_estado = rep(code_states[name]),
                 dados)

  # save in new dataframe
  bra_climate <- rbind(bra_climate, dados)

  # remove temporary object
  rm(dados)
}

# save data as csv
write.csv(bra_climate, "bra_climate.csv", row.names = FALSE)




