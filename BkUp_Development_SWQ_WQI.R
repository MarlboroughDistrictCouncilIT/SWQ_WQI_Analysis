#Clear environment
rm(list=ls())
#call libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(ggmap)
source("load_SWQ_functions.R") 
source("func_coordConversion_NZTM2000ToGeographic.R") 

#import data - Hilltop
source("func_queryHilltop.R")

# hilltopDataPath <- "//hydro2/hilltop/data/MDC Data.hts" ## to server **** safe start <- 29/03/07 ****
# hilltopProjectPath <- "U:/StaffResources/R-Z/She/Resources/General Project.hpr"

hilltopDataPath <- "C:/VM1/R_playground/SWQ_Project/SampleData/SoE_DataWQI_2007-2017.hts" 
hilltopProjectPath <- "C:/VM1/R_playground/SWQ_Project/SampleData/General Project.hpr"
collection <- "Data for WQI - all sites"
safeStartTime <- "0" #"15/02/2013"
safeEndTime <- "1"
ParameterFiltering <- "" #Project = SW SoE"

siteData <- func_queryHilltop(hilltopDataPath, hilltopProjectPath, collection, startTime = safeStartTime, endTime = safeEndTime, projectCodeQuery = ParameterFiltering)

siteData <- rename(siteData, Date=1, Temperature=2, Oxygen=3, Nitrate=4, Nitrite=5, Phosphorus=6, Ammonia=7, Ecoli=8, pH=9, Turbidity=10, Site=11, Easting=12, Northing=13)

siteData <- mutate(siteData, SINitrogen = Ammonia + Nitrite)


#define project values
paramColours <- c("SINitrogen" = "#95d680", "Phosphorus" = "#00cccc", "Oxygen" = "#00aaff", "Turbidity" = "#989898", "Ecoli" = "#ff9999", "Temperature" = "#ff9933", "pH" = "#dd99ff", "Ammonia" = "#dc65c0", "Nitrate" = "#b8b76b")

wqiClassFactorLevels <- c("Poor", "Marginal","Fair", "Good", "Excellent")
wqiColours <- c("Poor"="red", "Marginal"="orange", "Fair"="gold", "Good"="green","Excellent"="darkgreen")
# names(wqiColours) <- levels(siteData_ofinterest$WQI_class[[1]])

g_lines <- func_SWQ_guidelines_tibble()

#### Wrangle data ####

# func_SWQ_checktoguidelines_AmmoniaSummer <- function(data.in, guide.line){
#   date.month <- month(data.in[,1])
#   param <- data.in[,2]
#   if (between(date.month,4,10) == TRUE){
#     vector.out <- !between(vector.in,guide.line[1],guide.line[2])
#   }
#   
#   vector.out <- !between(vector.in,guide.line[1],guide.line[2])
#   return(vector.out)
# }

siteData_full <- siteData %>% 
  select(colnames(g_lines)) %>%
  map2_df(., g_lines, func_SWQ_checktoguidelines) %>% 
  setNames(paste0(names(.),"_boo")) %>% ##Line to change variable names
  bind_cols(siteData,.) %>% 
  mutate(SiteLat = map2_dbl(.$Northing, .$Easting, coord_NZTMToGeo , output = "lat")) %>% 
  mutate(SiteLon = map2_dbl(.$Northing, .$Easting, coord_NZTMToGeo, output = "lon"))

#export site data for shiny app
#### saveRDS(siteData_full,"C:/VM1/R_playground/SWQ_Project/SWQ_Shiny/SWQ_ShinyPOC/siteData_full.rds")

# test10 <- siteData2_full %>% 
#   filter(month(Date) >=4 & month(Date) <=10) %>% 
#   select(Date, Ammonia, Ammonia_boo) %>% 
  



# mutate(.[2]funs(func_SWQ_checktoguidelines,guide.line=c(0,0.75)))

  # mutate_df(Ammonia_boocheck = map_dbl(.,func_SWQ_checktoguidelines_AmmoniaSummer,guideline=c("0","0.76")))
  

#####select time span of data to consider for WQI#####

year_start <- 2008
time_span <- c(year_start,year_start+2)

# siteData_ofinterest  <- siteData_full %>% 
#   filter(between(year(.$Date),time_span[1],time_span[2])) %>% 
#   group_by(Site, Easting, Northing, SiteLat, SiteLon) %>% 
#   nest() %>% 
#   map_df(mutate(map(.$data, 
#                     func_SWQ_calcWQI, 
#                     guide.lines = g_lines),
#                 flatten_df)) %>% 
#   # map_df(siteData_ofinterest$WQI_calcs,flatten_df) %>% 
#   View
# 
# siteData_ofinterest <- siteData_ofinterest$WQI_calcs %>% 
#   map_df(.,flatten_df) %>% 
#   bind_cols(select(siteData_ofinterest,-WQI_calcs),.)
# 
# siteData_ofinterest$WQI_class <- siteData_ofinterest$WQI %>% 
#   map_chr(func_SWQ_WQIclass) %>% 
#   factor(wqiClassFactorLevels)


siteData_ofinterest  <- siteData_full %>%
  filter(between(year(.$Date),time_span[1],time_span[2])) %>%
  group_by(Site, Easting, Northing, SiteLat, SiteLon) %>%
  nest() %>%
  mutate(WQI_calcs = map(.$data, func_SWQ_calcWQI, guide.lines = g_lines))

siteData_ofinterest <- siteData_ofinterest$WQI_calcs %>%
  map_df(.,flatten_df) %>%
  bind_cols(select(siteData_ofinterest,-WQI_calcs),.)

siteData_ofinterest$WQI_class <- siteData_ofinterest$WQI %>%
  map_chr(func_SWQ_WQIclass) %>%
  factor(wqiClassFactorLevels)


### plot WQI for report ###
# 
# ## WQI by site plot
ggplot(siteData_ofinterest, aes(x = reorder(Site, WQI), y = WQI)) +
  geom_bar(stat = "identity", fill="blue", width = 0.5) +
  geom_hline(yintercept = c(45,65,80,95), colour = c("orange","gold","green", "darkgreen"), size = 0.5) +
  # scale_y_continuous(limits = c(0, 100)) +
  coord_flip() +
  theme_bw() +
  labs(x = "", y = "Water Quality Index") +
  scale_y_discrete(limits=c(seq(0,100,10)))
 
# ## Parameter by site plot
siteData_ofinterest %>%
  select(Site, WQI, contains("Ftot")) %>%
  select_all(~str_replace(., "Ftot_","")) %>%
  gather(key=Parameter,value = Ftot, 3:11) %>%
  mutate(Parameter = ordered(Parameter,levels=names(g_lines))) %>%
  ggplot(aes(x=reorder(Site, WQI), y=-1*Ftot, fill=Parameter)) +
    geom_bar(stat="identity", width = 0.5) +
    coord_flip() + theme_bw() + labs(x = "", y = "Parameter contribution") +
    scale_fill_manual(values = paramColours) +
    scale_y_discrete(limits=c(seq(-70,0,10)))

ggplot(data=siteData_ofinterest[1,],aes(x=2008, y=WQI)) + 
  geom_point(size=4, colour="blue") + 
  geom_bar()

siteData_ofinterest[1,] %>%
  select(Site, WQI, contains("Ftot")) %>%
  select_all(~str_replace(., "Ftot_","")) %>%
  gather(key=Parameter,value = Ftot, 3:11) %>%
  mutate(Parameter = ordered(Parameter,levels=names(g_lines))) %>%
  ggplot(aes(x=2008, y=-1*Ftot, fill=Parameter)) +
    geom_col(width = 0.5) +
    theme_bw() + labs(x = "", y = "Parameter deduction") +
    scale_fill_manual(values = paramColours) +
    # scale_y_discrete(limits=c(seq(0,100,10))) +
    scale_x_discrete(limits=c(seq(2007,2018,1))) +
    scale_y_continuous(position = "right", sec.axis = sec_axis(~.+100)) +
    geom_point(aes(x=2008, y=WQI-100), size=4, colour="blue") +
    geom_line
    




## plot WQI on map ##

if(exists("myMap")==FALSE){
  Marlborough <- c(lon=173.881,lat=-41.584)
  myMap <- get_map(location = Marlborough, source = "google", zoom=9)
}

# ggmap(myMap) +
#   geom_point(data = siteData_ofinterest, aes(x = SiteLon, y = SiteLat,  colour = WQI_class), size = 4) +
#   theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks = element_blank(), legend.position = c(0.125,0.1125)) +
#   labs(colour="WQI classification") + scale_colour_manual(values = wqiColours)
  
  

ggplot(siteData_ofinterest, aes(x=SiteLon, y=SiteLat, colour=WQI_class))+
  geom_point(size=4) +
  scale_colour_manual(values = wqiColours)


########  Surplus code
  
# siteData_ofinterest <- filter(siteData_full, between(year(siteData_full$Date),time_span[1],time_span[2])) #Select 
# siteData_ofinterest <- map(siteData_full, filter, between = year)
# siteData_ofinterest <- group_by(siteData_ofinterest, Site) %>% nest()
# siteData_ofinterest <- siteData_ofinterest %>%  unnest

####functions to perform F1, F2, F3 analysis####

# WQI_info <- map(siteData_ofinterest$data, func_SWQ_calcWQI, guide.lines = g_lines)

# WQI_info <- bind_cols(Site = siteData_ofinterest$Site[1],WQI_info) #Quick fix until nested structures introduced

# WQI_info_test <- tibble(
#   "Site" = c("Site 1","Site 2","Site 3","Site4"),
#   "WQI" = c(75,65,55,45),
#   "F1" = 1:4,
#   "F2" = 1:4,
#   "F3" = 1:4
# )

#WQI_info <- bind_rows(WQI_info,WQI_info_test)

# plot_colours <- c("SINitrogen" = "#95d680",
#                   "Phosphorus" = "#00cccc",
#                   "Oxygen" = "#00aaff",
#                   "Turbidity" = "#989898",
#                   "Ecoli" = "#ff9999",
#                   "Temperature" = "#ff9933",
#                   "pH" = "#dd99ff",
#                   "Ammonia" = "#000000",
#                   "Nitrate" = "#000000"
#                   )
# 
# colScale <- scale_fill_manual(name = "pltcols", values = plot_colours)
  
#plot WQI



# ggplot(WQI_info, aes(Site,WQI)) +
#   coord_flip() +
#   geom_bar(stat = "identity",fill="blue") +
#   geom_hline(yintercept = c(45,65,80,95), colour = c("red", "orange","green","purple"), size = 1)
#   
# #facet parameters
# 
# # yparameter="Phosphorus"
# ggplot(siteData_ofinterest,aes(x=Site,y=SINitrogen)) + 
#   coord_flip() +
#   geom_boxplot() 
#   
#   # geom_boxplot(aes(x=Site,y=pH), fill = "blue", colour = "blue") +
#   # facet_wrap(vars("Ecoli","pH")) 
# 
#   
# ggplot(siteData_ofinterest, aes(x=Site,y=Oxygen)) + 
#      coord_flip() +
#      geom_boxplot(fill = plot_colours["Oxygen"]) 
#      #scale_fill_manual(values = plot_colours, aesthetics = "colour")  
# 
# 
# #Visualise raw data in plot
# 
# yparameter <-  "SINitrogen"
# 
# ggplot(siteData_ofinterest) + 
#   aes_string(x="Date",y=yparameter) + geom_line() + geom_point() +
#   geom_hline(yintercept = pull(g_lines[,yparameter]), colour = "blue") +
#   geom_point(data=filter(siteData_ofinterest, !!as.symbol(paste0(yparameter,"_boo")) == "TRUE"),aes_string(x="Date",y=yparameter), colour="red",size=3)
# 
