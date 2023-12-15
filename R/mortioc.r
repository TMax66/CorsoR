library(tidyverse)
library(shiny)
library(shinydashboard)
library (DT)
library(plotly)
library(reshape2)
library(data.table)
library(dplyr)
library(ggplot2)
library(tibble)
library(lubridate)
library(scales)
library(gridExtra)
library(forecast)
library(TTR)
library(xts)
library(dygraphs)
library(datasets)
library(rpivotTable)
library(googlesheets)
####################################################################

dati<-gs_title("mortioc")
dati <-gs_read(dati, ws="dati")



dati$datap<-mdy(dati$datap)

dati$data<-as.Date(dati$datap)
dati<- mutate(dati, mese=paste(year(datap),'-',month(datap),sep=''))
dati$mese<-as.Date((paste(dati$mese,"-01",sep="")))


#dati$anno<-as.Date((paste(dati$anno,"-01","-01",sep="")))
dati$codaz<-casefold(dati$codaz, upper=TRUE)

dati<-dati %>% 
  group_by(specie,mese) %>% 
  summarise("morti"=sum(ncamp))
ov<-dati %>% 
  filter(specie=="OVINO")
cap<-dati %>% 
  filter(specie=="CAPRA")


#output$dygraph <- renderDygraph({
ovini<- xts(ov$morti, order.by = as.Date(ov$mese))
capre<-xts(cap$morti, order.by = as.Date(cap$mese))

morti<-cbind(ovini,capre)

dygraph(morti,ylab = "n.morti") %>% 
    dyAxis("y", label = "n.morti/mese",valueRange = c(0, 80)) %>% 
  dyRoller(rollPeriod = 6)

# %>% 
#     dyOptions(drawPoints = TRUE, pointSize = 2) 
# 
#   
#   


###by codaz###

dati<-dati %>% 
  group_by(codaz,specie,mese) %>% 
  summarise("morti"=sum(ncamp))

cap<-dati %>% 
  filter(specie=="CAPRA") 

x<-cap %>% 
  filter(codaz=="242BG019")
