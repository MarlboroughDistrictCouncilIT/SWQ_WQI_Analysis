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
g_lines <- func_SWQ_guidelines_tibble()

siteData_full <- siteData %>% 
  select(colnames(g_lines)) %>%
  map2_df(.,g_lines, func_SWQ_checktoguidelines) %>% 
  setNames(paste0(names(.),"_boo")) %>% ##Line to change variable names
  bind_cols(siteData,.)

#select time span of data to consider for WQI

time_span <- c(2013,2015)
siteData_ofinterest <- filter(siteData_full, between(year(siteData$Date),time_span[1],time_span[2])) #Select time span of interest


####functions to perform F1, F2, F3 analysis####

WQI_info <- func_SWQ_calcWQI(siteData_ofinterest,g_lines)


#Visualise raw data in plot


yparameter <-  "SINitrogen"

ggplot(siteData_ofinterest) + 
  aes_string(x="Date",y=yparameter) + geom_line() + geom_point() +
  geom_hline(yintercept = pull(g_lines[,yparameter]), colour = "blue") +
  geom_point(data=filter(siteData_ofinterest, !!as.symbol(paste0(yparameter,"_boo")) == "TRUE"),aes_string(x="Date",y=yparameter), colour="red",size=3)

