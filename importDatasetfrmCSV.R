siteData <- read.csv(paste(fpath,fname,".csv", sep=""), stringsAsFactors=FALSE)

siteData[,2] <- as.POSIXct(siteData[,2], format="%d-%b-%Y%t%H:%M:%S") #convert date time to R friendly format
siteData[,5] <- as.numeric(str_replace(siteData[,5],"<","")) #convert below lab senty charac to numeric

siteData <- cbind(siteData,(siteData[,4]+siteData[,5])) #calc Soluble Inorganic Nitrogen and append to results matrix

colnames(siteData) <- c("Site", 
                        "Date", 
                        "Nitrate", 
                        "Nitrite", 
                        "Ammonia",
                        "Phosphorus",
                        "Oxygen",
                        "Turbidity",
                        "Ecoli",
                        "Temperature",
                        "pH",
                        "SINitrogen")