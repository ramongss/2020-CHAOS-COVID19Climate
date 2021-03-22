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
            "foreach","iterators","doParallel","lmtest","wmtsa","httr",
            "jsonlite","magrittr","xlsx","devtools","inmetr")

sapply(packages,packs)

library(extrafont)
windowsFonts(Times = windowsFont("TT Times New Roman"))
library(ggplot2)
library(Cairo)

#Checa quantos núcleos existem
ncl<-detectCores();ncl

#Registra os clusters a serem utilizados
cl <- makeCluster(ncl-1);registerDoParallel(cl)
#############################Objects#######################################
Data_complete<-list()

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

#############################Climatological_Data - Fault##################################
## INMET Data Description

bdmep_description()

names<-c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO",
         "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", 
         "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

## SPLIT by state

bdmep_meta_state<-split(bdmep_meta,bdmep_meta$state)
names(bdmep_meta_state)<-names
lst[order(names(lst))]
bdmep_meta_state <- bdmep_meta_state[order(names(bdmep_meta_state))]; 


for(i in 23:length(bdmep_meta_state))
{
  ## Step 1: Get Stations Names 
  
  stations 		  <- 	bdmep_meta_state[[i]]$name
  stations_rows <- 	pmatch(stations, bdmep_meta$name)
  
  ## Step 2: Stations Codes
  
  stns_codes 		<- 	bdmep_meta[stations_rows, "id"] 
  
  ## Step 3: Start and End Dates
  
  start_date 		<- 	"25/02/2020"
  end_date 		  <- 	"28/04/2020"
  
  ## Step 4: Get the Data
  
  met_data 		<-	bdmep_import(id = stns_codes, sdate = start_date, edate = end_date, email = "matheus.dalmolin@gmail.com", passwd = "ijboyw67", verbose = FALSE)
  
  head(met_data)
  tail(met_data)
  
  ##========================##
  
  ## Step 1: Get Precipitation Information and Break by Day
  
  data.vars 		<- 	met_data[, c("date","id","prec","tmax","tmin","urmax")]
  data.vars$day <- 	cut(data.vars$date, breaks = "day")
  data.vars2 		<- 	data.vars[,-c(1)]
  
  ## Step 2: Aggregate the Total Precipitation and Organize by Date
  
  precipitation     		<- 	aggregate(prec ~ day + id, data = data.vars2, FUN = sum)
  maximal_temperature   <- 	aggregate(tmax ~ day + id, data = data.vars2, FUN = mean,na.rm=TRUE)
  minimum_temperature   <- 	aggregate(tmin ~ day + id, data = data.vars2, FUN = mean)
  humidity              <- 	aggregate(urmax ~ day + id, data = data.vars2, FUN = mean)
  
  prec_list<-split(precipitation,precipitation$id)
  prec_list<-lapply(prec_list, function(x) x[,-2])
  prec_join<-join_all(prec_list,"day")
  prec_join$prec_total<-apply(data.frame(prec_join[,-1]),1,sum, na.rm = TRUE)
  prec_final<-prec_join[,c("day","prec_total")]
  
  tmax_list<-split(maximal_temperature,maximal_temperature$id)
  tmax_list<-lapply(tmax_list, function(x) x[,-2])
  tmax_join<-join_all(tmax_list,"day")
  tmax_join$tmax_mean<-apply(data.frame(tmax_join[,-1]),1,mean, na.rm = TRUE)
  tmax_final<-tmax_join[,c("day","tmax_mean")]
  
  tmin_list<-split(minimum_temperature,minimum_temperature$id)
  tmin_list<-lapply(tmin_list, function(x) x[,-2])
  tmin_join<-join_all(tmin_list,"day")
  tmin_join$tmin_mean<-apply(data.frame(tmin_join[,-1]),1,mean, na.rm = TRUE)
  tmin_final<-tmin_join[,c("day","tmin_mean")]
  
  hum_list<-split(humidity,humidity$id)
  hum_list<-lapply(hum_list, function(x) x[,-2])
  hum_join<-join_all(hum_list,"day")
  hum_join$hum_mean<-apply(data.frame(hum_join[,-1]),1,mean, na.rm = TRUE)
  hum_final<-hum_join[,c("day","hum_mean")]
  
  climate_inputs     <-left_join(left_join(left_join(prec_final,tmax_final),tmin_final),hum_final)
  climate_inputs$ampl<-climate_inputs$tmax_mean-climate_inputs$tmin_mean
  climate_inputs$day<- as.Date(climate_inputs$day, format = '%Y-%m-%d')
  names(climate_inputs)[1]<-"date"
  
  #Join Climate and Covid data
  Data_complete[[i]]<-left_join(covid_list[[i]],climate_inputs,"date")
  cat('Data for state:',names[i],'colected! \n')
}


save(Data_complete,file="Data_CovidClima.RData")
setwd(DataDir)

load("Data_CovidClima.RData")

#Data of mobility
country_split<-split(Global_Mobility_Report,Global_Mobility_Report$country_region_code)

BRA<-data.frame(country_split$BR)
BRA_state<-split(BRA,BRA$sub_region_1)

states<-names(table(BRA$sub_region_1))
names<-c("DF","AC", "AL", "AM", "AP", "BA", "CE", "ES", "GO",
         "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", 
         "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
names(BRA_state)<-names
for(i in 1:length(BRA_state))
{
  BRA_state[[i]][,'sub_region_1']<-names[i]
  BRA_state[[i]][,'date']<-seq(from=as.Date("2020-02-15"), to=as.Date("2020-04-17"), by="month")
}


BRA_state<-BRA_state[sort(names)]

ClimateMobility<-list()
for(i in 1:length(BRA_state))
{
  if(is.null(Data_complete[[i]])==TRUE)
  {
    Data1<-BRA_state[[i]];Data2<-Data_complete[[1]]
    ClimateMobility[[i]]<-left_join(Data1,Data2,'date')
  }
  else
  {
    Data1<-BRA_state[[i]];Data2<-Data_complete[[i]]
    ClimateMobility[[i]]<-left_join(Data1,Data2,'date')
  }
}
  
###############################Climate-TOP5###############################

#TOP5 at 29/04

resumo<-do.call(rbind.data.frame,lapply(covid_list,function(x){tail(x[,-c(1,2)],1)}))
top5  <- head(resumo[order(resumo[,2],decreasing=TRUE),],5)

#Load Climate Data
setwd(DataDir)
dados_AM     <-read.table(file="climate_Amazonas_2020-04-30.csv"  ,header=TRUE,sep=";",dec=".")
dados_CE     <-read.table(file="climate_Ceara_2020-04-30.csv"     ,header=TRUE,sep=";",dec=".")
dados_PE     <-read.table(file="climate_Pernambuco_2020-04-30.csv",header=TRUE,sep=";",dec=".")
dados_RJ     <-read.table(file="climate_Rio_2020-04-30.csv"       ,header=TRUE,sep=";",dec=".")
dados_SP     <-read.table(file="climate_SaoPaulo_2020-04-30.csv"  ,header=TRUE,sep=";",dec=",")

climate_states<-list(AM=dados_AM,CE=dados_CE,PE=dados_PE,RJ=dados_RJ,SP=dados_SP)
vars          <-list()

# #Convert in Date
# for(i in 1:length(climate_states))
#   climate_states[[i]][,'data']<-as.Date(climate_states[[i]][,'data'], format = '%d/%m/%Y')
# 
# for(v in 1:(dim(climate_states[[i]])[2]-3))  
# {
#   d  <-aggregate(climate_states[[i]][,v+3] ~ data+codigo_estacao, data = climate_states[[i]], FUN = mean,na.rm=TRUE)
#   d  <-split(d,d$codigo_estacao)  
#   d  <-lapply(d, function(x) x[,-2])
#   d  <-join_all(d,'data')
#   d$mean<-apply(data.frame(d[,-1]),1,mean, na.rm = TRUE)
# }
# 

#Load Climate data for Brazil
setwd(DataDir)
dados_BRA     <-read.table(file="bra_covid_climate.csv"  ,header=TRUE,sep=",",dec=".")
dados_BRA_split<-split(dados_BRA,dados_BRA$state)

dados_USA     <-read.table(file="usa_covid_climate1.csv"  ,header=TRUE,sep=";",dec=",")
dados_USA_split<-split(dados_USA,dados_USA$state)

dados_list<-list(BRA=dados_BRA_split,USA=dados_USA_split)
descriptive<-lapply(dados_list,
                       function(x){lapply(x,
                                   function(y){apply(y[,4:6],2,summary)})})

descriptivef<-do.call(cbind.data.frame,descriptive)
write.xlsx(t(descriptivef),file="Descritivas_BRA_USA.xlsx")