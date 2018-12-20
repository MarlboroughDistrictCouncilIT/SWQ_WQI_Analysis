#Clear environment
# rm(list=ls())

require(Hilltop)
require(tidyverse)

zooTidy <- function(zoo){
  # zoo_site <- attr(zoo, "SiteName")
  zoo_dbl <- as.double(zoo)
  zoo_date <- attr(zoo, "index")
  zoo_measurement <- attr(zoo, "Measurement")
  return.tbl <- tibble(Date=zoo_date, Measurement=zoo_dbl)
  colnames(return.tbl) <- c("Date", zoo_measurement)
  return(return.tbl)
}

# servpath <- "C:/VM1/R_playground/SWQ_Project/SampleData/SoE_DataWQI_2007-2017.hts" #to Steffi dataset
# servpath <- "U:/StaffResources/R-Z/She/Resources/Shared/SoE_DataWQI_2007-2017.hts" ## to Steffi **** safe start <- 01/03/13 ****

htsPath <- "//hydro2/hilltop/data/MDC Data.hts" ## to server **** safe start <- 29/03/07 ****

hprPath <- "C:/VM1/R_playground/SWQ_Project/SampleData/General Project.hpr"
collectName <- "Data for WQI - all sites"

HTfile <- HilltopData(htsPath)

Collection <- GetCollection(HTfile, hprPath, collectName)
sList <- unique(Collection[1])
mList <- unique(Collection[2])


siteData <- tibble()

safeStartTime <- "27/02/2007"
safeEndTime <- "27/02/2008"

for(i in 1){
  sInfo <- SiteInfo(HTfile, sList[i,])
  print(paste0("Loading site ",sList[i,]))
  sData <- GetData(HTfile, sList[i,], mList[3,1], safeStartTime, safeEndTime, ParamFilter = "Project = SW SoE")
  
  if(length(sData)==0){
    print(paste0("Warning, site ", sList[i,]," returned no data when data were expected."))
  } else {
  
  # # sData <- sData %>%
  #   # map(.,gsub, pattern = "<", replacement = "") %>% 
  #   # map(.,gsub, pattern = ">", replacement = "") %>% 
  #   # map(.,zooTidy) %>%
  #   # reduce(full_join, by="Date")
  # 
  # # sData$Site <- sList[i,]
  # sData$Easting <- as.double(sInfo["Easting"])
  # sData$Northing <- as.double(sInfo["Northing"])

  siteData <- sData #bind_rows(siteData,sData)
  }
}
print("Loading sites complete")

# disconnect(HTfile)

  


# map_df(.,full_join, by=Date)
   
# map_df(.,broom::tidy) 
# 
# 
# #%>% 
#   View(siteData)



# 
# 
# 
# 
# 
# 
# for(i in 1:9){
#   circusData <- GetData(HTfile, sList[1,], mList[i,],"01/01/2013","31/12/2015", ParamFilter = "Project = SW SoE")
#   print(length(circusData))
# }
# 
# 
# for(i in 1){
#   # rawData <- GetData(HTfile, sList_text[i], mList_text[5:6],"22/02/2000","8/11/2018", ParamFilter = "Project = SW SoE")
#   rawData <- GetData(HTfile, sList_text[i], SWQ_mList[7,],"","1", ParamFilter = "Project = SW SoE")
#   siteData <- rawData
# }
# 
# 
# 
# HT_index2 <- data.frame()
# for(i in 1356){
#   # sInfo <- SiteInfo(HTfile, sList[i])
#   sInfo <- SiteInfo(HTfile, sList_text[i])
#   mList <- MeasurementList(HTfile,sList_text[i])
#   
#   # HT_index2 <- rbind(HT_index2, cbind(site=sList[i], mList))
#   
#   # mList$StartTime <- dmy_hms(mList$StartTime)
#   # mList$EndTime <- dmy_hms(mList$EndTime)
#   # data1 <- GetData(HTfile, sList[i], "Water Temperature (Field) [Water Temperature (Field)]", min(mList$StartTime, na.rm = T), max(mList$EndTime, na.rm = T))
#   # data2 <- GetData(HTfile, sList[i], "Item2 [Water Temperature (Field)]", min(mList$StartTime, na.rm = T), max(mList$EndTime, na.rm = T))
# }
# sInfo_testnotSWQ <- sInfo
# 
# 
# disconnect(HTfile)
# 
# SWQ_Collection <- GetCollection(HTfile,"U:/StaffResources/R-Z/She/Resources/General Project.hpr","Data for WQI - all sites")
# 
# SWQ_Collection <- nest(group_by(SWQ_Collection, Measurement))
# SWQ_mList <- SWQ_Collection["Measurement"]
# 
# test <- GetData(HTfile, )
