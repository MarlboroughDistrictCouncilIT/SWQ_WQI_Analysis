
hilltopDataPath <- "C:/VM1/R_playground/SWQ_Project/SampleData/SoE_DataWQI_2007-2017.hts"
hilltopProjectPath <- "C:/VM1/R_playground/SWQ_Project/SampleData/General Project.hpr"
collection <- "Data for WQI - all sites"
safeStartTime <- "15/02/2013"
safeEndTime <- "1" #14/02/2014"
ParameterFiltering <- ""#Project = SW SoE"



source("func_queryHilltop.R")
siteData_test <- func_queryHilltop(hilltopDataPath, hilltopProjectPath, collection, startTime = safeStartTime, endTime = safeEndTime, projectCodeQuery = ParameterFiltering)
