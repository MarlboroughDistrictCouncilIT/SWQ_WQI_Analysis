#Clear environment
rm(list=ls())
#call libraries
library(tidyverse)
library(lubridate)
source("load_SWQ_functions.R")

#import data
fpath="C:/VM1/R_playground/SWQ_Project/SampleData/"
fname="TaylorRiverAtRaibridge_allData_COPY"
source("importDatasetfrmCSV.R")


#define guideline values
guidelineMtx <- func_SWQ_guidelines()


# # guidelineMtx_Boo_exceedance <- cbind(siteData[,1:2],select(siteData,colnames(guidelineMtx)))
# 
# for (i in 3:length(guidelineMtx_Boo_exceedance[1,])){
#   i_parameter <- colnames(guidelineMtx_Boo_exceedance)[i]
#   guidelineMtx_Boo_exceedance[,i_parameter] <- between(guidelineMtx_Boo_exceedance[,i_parameter],guidelineMtx["Low",i_parameter],guidelineMtx["High",i_parameter])
# }

#:length(guidelineMtx_Boo_exceedance[1,])
for (i in 1:length(guidelineMtx[1,])){
  i_parameter <- colnames(guidelineMtx)[i]
  i_parametersymbol <- as.name(paste(as.character(i_parameter),"_boo",sep = ""))
  siteData <- mutate(siteData, !!i_parametersymbol := between(siteData[,i_parameter],guidelineMtx["Low",i_parameter],guidelineMtx["High",i_parameter]))
}

time_span <- c(2013,2015)
siteData_ofinterest <- filter(siteData, between(year(siteData$Date),time_span[1],time_span[2])) #Select time span of interest

#functions to perform F1, F2, F3 analysis

#F1
# siteData_ofinterest.F1 <- 

#calc WQI

#plot WQI, 

#Calculate Steffi's further analysis


#Visualise raw data in plot


yparameter <-  "pH"

ggplot(siteData_ofinterest) + 
  aes_string(x="Date",y=yparameter) + geom_line() + geom_point() +
  geom_hline(yintercept = guidelineMtx[,yparameter], colour = "blue") +
  geom_point(data=filter(siteData_ofinterest, !!as.symbol(paste(yparameter,"_boo",sep="")) == "FALSE"),aes_string(x="Date",y=yparameter), colour="red",size=3)

####
# ggplot(filter(plotdata, pH1 == "FALSE")) +
#   geom_point(aes(x=Date,y=pH), colour="red") +
#   geom_hline(yintercept = guidelineMtx["Low",yparameter], colour = "blue") +
#   geom_hline(yintercept = guidelineMtx["High",yparameter], colour = "blue")
  
