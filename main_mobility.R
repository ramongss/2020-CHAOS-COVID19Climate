rm(list = ls())
#setwd("C:/Users/Usuario/Dropbox/COVID-ITALY-BRAZIL")
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
source("elm_caret.R")
source("Metricas.R")

#library(devtools)
#install_github('lhmet/inmetr')

packages<-c("forecast","cowplot","Metrics","caret","elmNNRcpp","tcltk","TTR",
            "foreach","iterators","doParallel","plyr","wmtsa","httr",
            "jsonlite","magrittr","xlsx","devtools","tidyverse","tidycovid19")

sapply(packages,packs)

library(extrafont)
windowsFonts(Times = windowsFont("TT Times New Roman"))
library(ggplot2)
library(Cairo)

#Checa quantos núcleos existem
ncl<-detectCores();ncl

#Registra os clusters a serem utilizados
cl <- makeCluster(ncl-1);registerDoParallel(cl)

########################Get Mobility Data##############################

df <- download_merged_data(cached = TRUE, silent = TRUE)

df %>%
  filter(iso3c == "USA") %>%
  mutate(
    new_cases = confirmed - lag(confirmed),
    ave_new_cases = rollmean(new_cases, 7, na.pad=TRUE, align="right")
  ) %>%
  filter(!is.na(new_cases), !is.na(ave_new_cases)) %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = new_cases), stat = "identity", fill = "lightblue") +
  geom_line(aes(y = ave_new_cases), color ="red") +
  theme_minimal()

merged <- download_merged_data(cached = TRUE, silent = TRUE)

plot_covid19_spread(
  merged, highlight = c("BRA", "USA","GBR"),
  intervention = "lockdown", edate_cutoff = 60
)

df <- download_google_cmr_data(cached = TRUE, silent = TRUE)

BRA<- df[[2]] %>%
      filter(iso3c == "BRA") 

BRA_state <- split(BRA,BRA$region)
names_states<-c("DF","AC", "AL", "AM", "AP", "BA", "CE", "ES", "GO",
         "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", 
         "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
names(BRA_state)<-names_states
for(i in 1:length(BRA_state))
  BRA_state[[i]][,'region']<-names[i]

#############################Covid-Data###########################################
# Load the data from API
covid <- 
  GET(url = "https://brasil.io/api/dataset/covid19/caso/data/?place_type=state") %>% 
  content %>%
  toJSON %>%
  fromJSON

# Separate the data
covid <- data.frame(
  date        = covid$results$date      %>% unlist %>% as.Date,
  state       = covid$results$state     %>% unlist,
  confirmed   = covid$results$confirmed %>% unlist,
  deaths      = covid$results$deaths    %>% unlist
)

# Order by state and date
covid <- covid[order(covid$state, covid$date),]; rownames(covid) <- NULL

# Plot confirmed cases by state
covid %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = confirmed)) +
  facet_wrap(~state, scales = "free")+
  scale_x_date(date_labels = "%d/%m")

# States by list
covid_list <- covid %>%
  split(covid$state)

#Insert incidence
for(i in 1:length(covid_list))
{
  covid_list[[i]]$confirmed_incidence <-rep(0,times=dim(covid_list[[i]])[1])
  covid_list[[i]]$deaths_incidence<-rep(0,times=dim(covid_list[[i]])[1])
  
  for(j in 1:dim(covid_list[[i]])[1])
  {
    if(j==1)
    {
      covid_list[[i]][j,"confirmed_incidence"] <-covid_list[[i]][j,"confirmed"]
      covid_list[[i]][j,"deaths_incidence"]<-covid_list[[i]][j,"deaths"]
    }
    else
    {
      covid_list[[i]][j,"confirmed_incidence"] <-covid_list[[i]][j,"confirmed"]-covid_list[[i]][j-1,"confirmed"]
      covid_list[[i]][j,"deaths_incidence"]<-covid_list[[i]][j,"deaths"]-covid_list[[i]][j-1,"deaths"]
    }
  }
}

states<-c('SP','RJ','AM','CE','PE','PR','SC')
data_covid<-covid_list[states]
data_mobil<-BRA_state[states]
data_join <-list()
aux       <-list()

for(i in 1:length(data_covid))
{
  aux[[i]]      <-list(data_covid[[i]],data_mobil[[i]])
  data_join[[i]]<-join_all(aux[[i]],"date")
}


data_join<-lapply(data_join, function(x) x[!(names(x) %in% c('iso3c','region','timestamp','deaths','confirmed_incidence','deaths_incidence'))])
data_join<-lapply(data_join, function(x) x[complete.cases(x),])

corr<-lapply(data_join, function(x){res2<-rcorr(as.matrix(x[,-c(1,2)]))
                                    flattenCorrMatrix(res2$r, res2$P)})
names(corr)<-c('SP','RJ','AM','CE','PE','PR','SC')

for(i in 1:length(corr))
{
  corr[[i]]$Sig<-ifelse(corr[[i]][,'p']<=0.1,"Yes","No")
}

corr<-do.call(cbind.data.frame,corr)
write.xlsx(corr,file="corr_states.xlsx")

