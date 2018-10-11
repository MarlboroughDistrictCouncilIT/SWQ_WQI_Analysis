fname <- list.files(path = fpath, pattern = "^SWQData")
siteData <- tibble()

for (i in 1:length(fname)){
  import <- read_excel(paste0(fpath,fname[i]), col_names = FALSE, skip = 2)
  siteData <- bind_rows(siteData,import)
}

# siteData[,5] <- as.numeric(str_replace(siteData[,5],"<","")) #convert below lab senty charac to numeric
siteData <- siteData %>% 
  select(1:13)

colnames(siteData) <- c("Site", "Easting", "Northing", "Date", "Temperature", "Oxygen", "pH", "Turbidity", "Ecoli", "Phosphorus", "Ammonia", "Nitrate", "Nitrite")

siteData <- mutate(siteData, SINitrogen = Ammonia + Nitrite)
