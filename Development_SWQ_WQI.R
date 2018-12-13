#Clear environment
rm(list=ls())
#call libraries
library(tidyverse)
library(lubridate)
library(readxl)
# library(ggmap)
source("load_SWQ_functions.R") 
# source("func_coordConversion_NZTM2000ToGeographic.R") 

#import data - Hilltop
source("func_queryHilltop.R")

# hilltopDataPath <- "//hydro2/hilltop/data/MDC Data.hts" ## to server **** safe start <- 29/03/07 ****
# hilltopProjectPath <- "U:/StaffResources/R-Z/She/Resources/General Project.hpr"

hilltopDataPath <- "C:/VM1/R_playground/SWQ_Project/SampleData/SoE_DataWQI_2007-2017.hts"
hilltopProjectPath <- "C:/VM1/R_playground/SWQ_Project/SampleData/General Project.hpr"
collection <- "Data for WQI - all sites"
safeStartTime <- "0" # "15/02/2013"
safeEndTime <- "1" #14/02/2014"
ParameterFiltering <- "" # "Project = SW SoE"

siteData <- func_queryHilltop(hilltopDataPath, hilltopProjectPath, collection, startTime = safeStartTime, endTime = safeEndTime, projectCodeQuery = ParameterFiltering)

siteData <- rename(siteData, Date=1, Temperature=2, Oxygen=3, Nitrate=4, Nitrite=5, Phosphorus=6, Ammonia=7, Ecoli=8, pH=9, Turbidity=10, Site=11, Easting=12, Northing=13)

siteData <- mutate(siteData, SINitrogen = Ammonia + Nitrite)

# 
# #define project values
paramColours <- c("SINitrogen" = "#95d680", "Phosphorus" = "#00cccc", "Oxygen" = "#00aaff", "Turbidity" = "#989898", "Ecoli" = "#ff9999", "Temperature" = "#ff9933", "pH" = "#dd99ff", "Ammonia" = "#dc65c0", "Nitrate" = "#b8b76b")
 
wqiClassFactorLevels <- c("Poor", "Marginal","Fair", "Good", "Excellent")
wqiColours <- c("Poor"="red", "Marginal"="orange", "Fair"="gold", "Good"="green","Excellent"="darkgreen")
# # names(wqiColours) <- levels(siteData_ofinterest$WQI_class[[1]])
 
g_lines <- func_SWQ_guidelines_tibble()

dfCoords <- func_NZTM_WGS84(siteData$Easting, siteData$Northing)

siteData$SiteLon <- dfCoords[,1]
siteData$SiteLat <- dfCoords[,2]
 
# #### Wrangle data ####

siteData_full <- siteData %>%
  select(colnames(g_lines)) %>%
  map2_df(., g_lines, func_SWQ_checktoguidelines) %>%
  setNames(paste0(names(.),"_boo")) %>% ##Line to change variable names
  bind_cols(siteData,.) 



# %>%
#   mutate(SiteLat = map2_dbl(.$Easting, .$Northing, func_NZTM_WGS84 , output = "lat")) %>%
#   mutate(SiteLon = map2_dbl(.$Easting, .$Northing, func_NZTM_WGS84, output = "lon"))




# siteData_full$SiteLat=
# 
# %>%
#   mutate(SiteLat = map2_dbl(.$Easting, .$Northing, func_NZTM_WGS84 , output = "lat")) %>%
#   mutate(SiteLon = map2_dbl(.$Easting, .$Northing, func_NZTM_WGS84, output = "lon"))

 
#export sitedata_full for shiny app
# saveRDS(siteData_full,"C:/VM1/R_playground/SWQ_Project/SWQ_Shiny/SWQ_ShinyPOC/siteData_full.rds")


#####select time span of data to consider for WQI#####

year_start <- 2007:(year(now())-2)


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

# siteData_WQI <- tibble()
for(i in 1:length(year_start)){
  if(i==1){siteData_WQI <- tibble()}
  siteData_WQI  <- siteData_full %>%
    filter(between(year(.$Date), year_start[i], year_start[i]+2)) %>%
    mutate(yearStart=year_start[i]) %>%
    bind_rows(siteData_WQI,.)
}

siteData_WQI <- siteData_WQI %>%
    group_by(yearStart, Site, Easting, Northing, SiteLat, SiteLon) %>%
    nest() %>%
    mutate(WQI_calcs = map(.$data, func_SWQ_calcWQI, guide.lines = g_lines)) #add yearstart variable here

siteData_WQI <- siteData_WQI$WQI_calcs %>%
  map_df(.,flatten_df) %>%
  bind_cols(select(siteData_WQI,-WQI_calcs),.)

siteData_WQI$WQI_class <- siteData_WQI$WQI %>%
  map_chr(func_SWQ_WQIclass) %>%
  factor(wqiClassFactorLevels)

#export siteData_WQI for shiny app
# saveRDS(siteData_WQI,"C:/VM1/R_playground/SWQ_Project/SWQ_Shiny/SWQ_ShinyPOC/siteData_WQI.rds")

# siteData_WQI %>% 
#   filter(yearStart==2013) %>% 
#   mutate(WQI=round(WQI,1)) %>% 
#   select(yearStart,Site,WQI) %>% 
#   View


### plot WQI for report ###
#
# ## WQI by site plot
filter(siteData_WQI,yearStart==2013) %>%
  ggplot(aes(x = reorder(Site, WQI), y = WQI)) +
    geom_bar(stat = "identity", fill="blue", width = 0.5) +
    geom_hline(yintercept = c(45,65,80,95), colour = c("orange","gold","green", "darkgreen"), size = 0.5) +
    # scale_y_continuous(limits = c(0, 100)) +
    coord_flip() +
    theme_bw() +
    labs(x = "", y = "Water Quality Index") +
    scale_y_discrete(limits=c(seq(0,100,10)))

# ## Parameter by site plot
siteData_WQI %>%
  filter(yearStart==2013) %>%
  select(Site, WQI, contains("Ftot")) %>%
  select_all(~str_replace(., "Ftot_","")) %>%
  gather(key=Parameter,value = Ftot, 3:11) %>%
  mutate(Parameter = ordered(Parameter,levels=names(g_lines))) %>%
  ggplot(aes(x=reorder(Site, WQI), y=-1*Ftot, fill=Parameter)) +
    geom_bar(stat="identity", width = 0.5) +
    coord_flip() + theme_bw() + labs(x = "", y = "Parameter contribution") +
    scale_fill_manual(values = paramColours) +
    scale_y_discrete(limits=c(seq(-70,0,10)))


# ## WQI and parameter deduction trend
siteData_WQI %>%
  # filter(Site=="Mill Creek at Ormonds") %>%
  filter(Site=="Waima (Ure) River at SH1 Bridge") %>%
  # filter(Site=="Rai River at Rai Falls") %>%
  select(yearStart, Site, WQI, completeSet, contains("Ftot")) %>%
  mutate(alpha=factor(completeSet, levels = c(T,F))) %>%
  # mutate(alpha=completeSet*0.6+0.4) %>% 
  select_all(~str_replace(., "Ftot_","")) %>% 
  gather(key=Parameter,value = Ftot, Turbidity:Nitrate) %>%
  mutate(Parameter = ordered(Parameter,levels=names(g_lines))) %>% 
  ggplot(aes(x=yearStart, y=-1*Ftot)) +
    
    theme_bw() + labs(x = "Year", y = "Parameter deduction") +
    
    scale_y_continuous(position = "right", limits = c(-100,0), 
                       sec.axis = sec_axis(~.+100, name="Water Quality Index")) +
    scale_x_continuous(breaks = seq(2000,2025,1)) +
    
    scale_fill_manual(values = paramColours, labels = names(paramColours), name =  "Parameters") +
    scale_colour_manual(values = "blue", labels = "WQI", name = NULL) +
  
    scale_alpha_manual(name = "Dataset completeness", values = c(1,0.6), label = c("Full", "Partial"), drop = FALSE) +
    
    geom_col(aes(fill=Parameter, alpha=alpha), width = 0.5) +
    geom_point(aes(y=WQI-100, colour="WQI"), size=4) +
    geom_line(aes(y=WQI-100, colour="WQI"), size=1.5) 
    
    
  


# ## plot parameter over time
siteData_full %>%
  filter(Site=="Mill Creek at Ormonds") %>% #Awatere River at River Mouth
  arrange(Date) %>%
  ggplot(aes(x=Date, y=Turbidity, col="Turbidity")) +
    geom_line() + 
    geom_point() + 
    theme_bw() +
    scale_y_continuous(expand = expand_scale(mult = c(0.1,0.1))) + 
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y", expand = expand_scale(add = c(0.5,0.5))) +
    scale_colour_manual(values=paramColours["Turbidity"], name="Parameter", labels="SINitrogen")  


      
  
    labs(x = "Year", y = "SINitrogen", title = paste0("SINitrogen variation with time at Mill Creek at Ormonds site"))
    
    
    
  


(fill=paramColours["SINitrogen"])
  
    
      scale_x_datetime(date_breaks = "1 year", 
                     date_labels = "%Y",
                     expand = expand_scale(mult = c(0.1,0.1))) +
    scale_y_continuous(expand = expand_scale(mult = c(0,0.1)))
  
  #coord_cartesian(#ylim = c(min(ploty),1E5), 
                    #xlim = c(as_datetime("2007-01-01"), ceiling_date(max(siteData_full$Date),unit = "year")), 
   #                 expand=FALSE)
  
# 
# ## plot WQI on map ##
# 
# # if(exists("myMap")==FALSE){
# #   Marlborough <- c(lon=173.881,lat=-41.584)
# #   myMap <- get_map(location = Marlborough, source = "google", zoom=9)
# # }
# 
# # ggmap(myMap) +
# #   geom_point(data = siteData_ofinterest, aes(x = SiteLon, y = SiteLat,  colour = WQI_class), size = 4) +
# #   theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks = element_blank(), legend.position = c(0.125,0.1125)) +
# #   labs(colour="WQI classification") + scale_colour_manual(values = wqiColours)
# 
# 
# 
# ggplot(siteData_ofinterest, aes(x=SiteLon, y=SiteLat, colour=WQI_class))+
#   geom_point(size=4) +
#   scale_colour_manual(values = wqiColours)
# 
# 
# ########  Surplus code
# 
# # siteData_ofinterest <- filter(siteData_full, between(year(siteData_full$Date),time_span[1],time_span[2])) #Select
# # siteData_ofinterest <- map(siteData_full, filter, between = year)
# # siteData_ofinterest <- group_by(siteData_ofinterest, Site) %>% nest()
# # siteData_ofinterest <- siteData_ofinterest %>%  unnest
# 
# ####functions to perform F1, F2, F3 analysis####
# 
# # WQI_info <- map(siteData_ofinterest$data, func_SWQ_calcWQI, guide.lines = g_lines)
# 
# # WQI_info <- bind_cols(Site = siteData_ofinterest$Site[1],WQI_info) #Quick fix until nested structures introduced
# 
# # WQI_info_test <- tibble(
# #   "Site" = c("Site 1","Site 2","Site 3","Site4"),
# #   "WQI" = c(75,65,55,45),
# #   "F1" = 1:4,
# #   "F2" = 1:4,
# #   "F3" = 1:4
# # )
# 
# #WQI_info <- bind_rows(WQI_info,WQI_info_test)
# 
# # plot_colours <- c("SINitrogen" = "#95d680",
# #                   "Phosphorus" = "#00cccc",
# #                   "Oxygen" = "#00aaff",
# #                   "Turbidity" = "#989898",
# #                   "Ecoli" = "#ff9999",
# #                   "Temperature" = "#ff9933",
# #                   "pH" = "#dd99ff",
# #                   "Ammonia" = "#000000",
# #                   "Nitrate" = "#000000"
# #                   )
# #
# # colScale <- scale_fill_manual(name = "pltcols", values = plot_colours)
# 
# #plot WQI
# 
# 
# 
# # ggplot(WQI_info, aes(Site,WQI)) +
# #   coord_flip() +
# #   geom_bar(stat = "identity",fill="blue") +
# #   geom_hline(yintercept = c(45,65,80,95), colour = c("red", "orange","green","purple"), size = 1)
# #
# # #facet parameters
# #
# # # yparameter="Phosphorus"
# # ggplot(siteData_ofinterest,aes(x=Site,y=SINitrogen)) +
# #   coord_flip() +
# #   geom_boxplot()
# #
# #   # geom_boxplot(aes(x=Site,y=pH), fill = "blue", colour = "blue") +
# #   # facet_wrap(vars("Ecoli","pH"))
# #
# #
# # ggplot(siteData_ofinterest, aes(x=Site,y=Oxygen)) +
# #      coord_flip() +
# #      geom_boxplot(fill = plot_colours["Oxygen"])
# #      #scale_fill_manual(values = plot_colours, aesthetics = "colour")
# #
# #
# # #Visualise raw data in plot
# #
# # yparameter <-  "SINitrogen"
# #
# # ggplot(siteData_ofinterest) +
# #   aes_string(x="Date",y=yparameter) + geom_line() + geom_point() +
# #   geom_hline(yintercept = pull(g_lines[,yparameter]), colour = "blue") +
# # #   geom_point(data=filter(siteData_ofinterest, !!as.symbol(paste0(yparameter,"_boo")) == "TRUE"),aes_string(x="Date",y=yparameter), colour="red",size=3)
# # # 
# 
# 
# leaflet(siteData_WQI) %>% 
#   addTiles() %>% 
#   addCircleMarkers(lng = ~SiteLon ,
#                    lat = ~SiteLat,
#                    popup = ~paste0("<h4>",Site,"</h4>",
#                                    "<p style=\"font-size:1.3em\">WQI: ", round(WQI, digits = 1),"</p>"),
#                    color = "black",
#                    weight = 2,
#                    radius = 6)
