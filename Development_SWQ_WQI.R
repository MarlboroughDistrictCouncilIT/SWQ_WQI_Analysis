#Clear environment
rm(list=ls())
#call libraries
library(tidyverse)
library(lubridate)
library(readxl)
source("load_SWQ_functions.R")


#import data
fpath="C:/VM1/R_playground/SWQ_Project/SampleData/"
fname="TaylorRiverAtRaibridge_allData_COPY"
source("importDatasetfrmXLSX.R")

#define guideline values
g_lines <- func_SWQ_guidelines_tibble()

siteData_full <- siteData %>% 
  select(colnames(g_lines)) %>%
  map2_df(.,g_lines, func_SWQ_checktoguidelines) %>% 
  setNames(paste0(names(.),"_boo")) %>% ##Line to change variable names
  bind_cols(siteData,.)

# siteData_full <- siteData %>% 
#   group_by(Site) %>% 
#   nest()

#select time span of data to consider for WQI

year_start <- 2013
time_span <- c(year_start,year_start+2)

source("load_SWQ_functions.R")

siteData_ofinterest <- siteData_full %>% 
  filter(between(year(.$Date),time_span[1],time_span[2])) %>% 
  group_by(Site, Easting, Northing) %>% 
  nest() %>% 
  mutate(WQI_info = map(.$data, func_SWQ_calcWQI, guide.lines = g_lines))

View(siteData_ofinterest$WQI_info[[6]])
View(siteData_ofinterest$data[[6]])

# siteData_ofinterest <- filter(siteData_full, between(year(siteData_full$Date),time_span[1],time_span[2])) #Select 
# siteData_ofinterest <- map(siteData_full, filter, between = year)
# siteData_ofinterest <- group_by(siteData_ofinterest, Site) %>% nest()
# siteData_ofinterest <- siteData_ofinterest %>%  unnest

####functions to perform F1, F2, F3 analysis####

# WQI_info <- map(siteData_ofinterest$data, func_SWQ_calcWQI, guide.lines = g_lines)

siteData_ofinterest <- siteData_ofinterest %>% 
  mutate(WQI_info = map(siteData_ofinterest$data, func_SWQ_calcWQI, guide.lines = g_lines))
# WQI_info <- bind_cols(Site = siteData_ofinterest$Site[1],WQI_info) #Quick fix until nested structures introduced

# WQI_info_test <- tibble(
#   "Site" = c("Site 1","Site 2","Site 3","Site4"),
#   "WQI" = c(75,65,55,45),
#   "F1" = 1:4,
#   "F2" = 1:4,
#   "F3" = 1:4
# )

WQI_info <- bind_rows(WQI_info,WQI_info_test)

plot_colours <- c("SINitrogen" = "#95d680",
                  "Phosphorus" = "#00cccc",
                  "Oxygen" = "#00aaff",
                  "Turbidity" = "#989898",
                  "Ecoli" = "#ff9999",
                  "Temperature" = "#ff9933",
                  "pH" = "#dd99ff",
                  "Ammonia" = "#000000",
                  "Nitrate" = "#000000"
                  )

colScale <- scale_fill_manual(name = "pltcols", values = plot_colours)
  
#plot WQI

ggplot(WQI_info, aes(Site,WQI)) +
  coord_flip() +
  geom_bar(stat = "identity",fill="blue") +
  geom_hline(yintercept = c(45,65,80,95), colour = c("red", "orange","green","purple"), size = 1)
  
#facet parameters

# yparameter="Phosphorus"
ggplot(siteData_ofinterest,aes(x=Site,y=SINitrogen)) + 
  coord_flip() +
  geom_boxplot() 
  
  # geom_boxplot(aes(x=Site,y=pH), fill = "blue", colour = "blue") +
  # facet_wrap(vars("Ecoli","pH")) 

  
ggplot(siteData_ofinterest, aes(x=Site,y=Oxygen)) + 
     coord_flip() +
     geom_boxplot(fill = plot_colours["Oxygen"]) 
     #scale_fill_manual(values = plot_colours, aesthetics = "colour")  


#Visualise raw data in plot

yparameter <-  "SINitrogen"

ggplot(siteData_ofinterest) + 
  aes_string(x="Date",y=yparameter) + geom_line() + geom_point() +
  geom_hline(yintercept = pull(g_lines[,yparameter]), colour = "blue") +
  geom_point(data=filter(siteData_ofinterest, !!as.symbol(paste0(yparameter,"_boo")) == "TRUE"),aes_string(x="Date",y=yparameter), colour="red",size=3)

