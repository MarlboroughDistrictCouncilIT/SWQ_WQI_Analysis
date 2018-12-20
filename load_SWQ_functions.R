func_SWQ_guidelines_tibble <- function(){
  tibble(
    Turbidity = c(low=1E-5, high=5.6), #NTU
    SINitrogen = c(low=0, high=0.165), #mg/L as N
    Phosphorus = c(low=0, high=0.015), #mg/L
    Ecoli = c(low=0, high=550), #units/100mL
    pH = c(low=6.7, high=7.8), #unitless
    Oxygen = c(low=70, high=150), #%
    Temperature = c(low=0, high=21.5), #degC
    Ammonia=c(low=0, high=0.2), #mg/L as N *the high/low is set in call to check data to guideline
    Nitrate=c(low=0, high=2.4) #mg/L
  )
}

func_SWQ_checktoguidelines <- function(vector.in,guide.line){
  vector.out <- !between(vector.in,guide.line[1],guide.line[2])
  return(vector.out)
}

func_SWQ_WQIclass <- function(WQI){
  if (WQI >= 95 ){
    WQI_class <- "Excellent"
  } else if (WQI >= 80){
    WQI_class <- "Good"
  } else if (WQI >= 65){
    WQI_class <- "Fair"
  } else if (WQI >= 45){
    WQI_class <- "Marginal"
  } else {
    WQI_class <- "Poor"
  }
  
  return(WQI_class)
}

func_SWQ_WQIcolour <- function(WQI){
  if (WQI >= 95 ){
    WQI_colour <- "Excellent"
  } else if (WQI >= 80){
    WQI_colour <- "Good"
  } else if (WQI >= 65){
    WQI_colour <- "Fair"
  } else if (WQI >= 45){
    WQI_colour <- "Marginal"
  } else {
    WQI_colour <- "Poor"
  }
  
  return(WQI_class)
}

func_SWQ_calcWQI <- function(tbl.in,guide.lines){
  
  boos.df <- tbl.in %>%
    select(contains("_boo"))
  
  names(boos.df) <- names(guide.lines)
  
  func_totaltests <- function(tests.vector){sum(!is.na(tests.vector))}
  
  total.tests <- boos.df %>% 
    map_dbl(.,func_totaltests)
  
  # F2 <- boos.df %>% 
  #   map_dbl(.,sum,na.rm = "True")/sum(total.tests) *100
  # 
  F2 <- boos.df %>% 
    map_dbl(.,sum,na.rm = "True")/sum(total.tests) *100 #%>% 
  
  # print(names(F2))
  # print(names(guide.lines))
  #set_names(.,nm = names(guide.lines))
  # return(F2)
  
  F1 <- F2 %>% {ifelse(.>0,1,0)}
  F1 <- F1/length(F1) *100
  
  #F2 <- sum(F2) ## Comment this line out if wanting parameter specific components of 
  
  func_SWQ_calcF3 <- function(vector.in,vector.boo,guide.lines){
    true.rows=which(vector.boo == T, arr.ind = T)
    vector.out <- double(length(vector.in))
    for(i in true.rows){
      if (vector.in[i]>guide.lines[2]){
        vector.out[i] <- (vector.in[i]/guide.lines[2])-1
      } 
      else if (vector.in[i]<guide.lines[1]){
        vector.out[i] <- (guide.lines[1]/vector.in[i])-1
      }
    }
    vector.out <- sum(vector.out,na.rm = "True")
    return(vector.out)
  }
  
  excursion <- pmap_dbl(
    list(select(tbl.in,one_of(colnames(guide.lines))),
         select(tbl.in,one_of(paste0(colnames(guide.lines),"_boo"))),
         guide.lines),
    func_SWQ_calcF3
  )
  #nse <- sum(excursion)/sum(total.tests)
   nse <- (excursion)/sum(total.tests)
  
  F3_param <- nse/(0.01*nse+0.01)
  F3 <- nse/(0.01*sum(nse)+0.01)
  
  WQI <- 100-(sqrt((sum(F1))^2 + (sum(F2))^2 + (sum(F3))^2)/1.732) 
  
  X1 <-  sum(F1) + sum(F2) + sum(F3)
  X2 <- 100 - WQI
  
  ratio <- X2/X1
  
  F1_param <- F1 * ratio
  F2_param <- F2 * ratio
  F3_param <- F3 * ratio
  Ftot_param <- (F1+F2+F3) * ratio #add this
  
  names(F1_param) <- paste0("F1_",names(F1_param))
  names(F2_param) <- paste0("F2_",names(F2_param))
  names(F3_param) <- paste0("F3_",names(F3_param))
  names(Ftot_param) <- paste0("Ftot_",names(Ftot_param)) #add this
  
  # print(nrow(tbl.in))
  completeSet <- tbl.in %>% {ifelse(nrow(.)<30,FALSE,TRUE)}
  
  return.list <- list(WQI=WQI, completeSet=completeSet, F1=F1_param, F2=F2_param, F3=F3_param, Ftot=Ftot_param) #add last
  
  return(return.list)
}

func_NZTM_WGS84 <- function(Easting, Northing, output = "latlon"){
  require(sp)
  
  epsg2193 <- "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  siteCoords <- SpatialPoints(cbind.data.frame(x=Easting, y=Northing), proj4string = CRS(epsg2193))
  coordsNew <- coordinates(spTransform(siteCoords, CRS(wgs84)))
  # print(coordsNew)
  unname(coordsNew)
  lon = coordsNew[,1]
  lat = coordsNew[,2]
  
  
  if(output == "lat"){
    return(lat)
  } else if (output == "lon"){
    return(lon)
  } else {
    return(coordsNew)
  }
}

func_SWQ_siteShortNames <- function(){
  c("Are Are Creek at Kaituna Tuamarina Track" = "Are Are Creek",
    "Awatere River at Awapiri" = "Mid Awatere River",
    "Awatere River at River Mouth" = "Lower Awatere River",
    "Black Birch Stream at Awatere Intake" = "Black Birch Stream",
    "Branch River at Weir Intake" = "Branch River",
    "Cullen Creek at Road Bridge" = "Cullens Creek",
    "Doctors Creek Upstream Taylor" = "Doctors Creek",
    "Duncan Stream at Outlet" = "Linkwater Stream",
    "Flaxbourne River at Quarry" = "Flaxbourne River",
    "Goulter River at Horseshoe Bend" = "Goulter River",
    "Graham River at Road Bridge" = "Graham River",
    "Kaituna River at Higgins Bridge" = "Kaituna River",
    "Kenepuru Stream at Kenepuru Head" = "Kenepuru Stream",
    "Mill Creek at Ormonds" = "Mill Creek",
    "Murphys Creek at Nelson Street" = "Murphys Creek",
    "Omaka River at Hawkesbury Road Bridge" = "Omaka River",
    "Onamalutu River at Northbank Road" = "Ohinemahuta River",
    "Opawa River at Hammerichs Road" = "Mid Opaoa River",
    "Opawa River at Swamp Road" = "Lower Opaoa River",
    "Opouri River at Tunakino Valley Road" = "Opouri River",
    "Pelorus River at Fishermans Flat" = "Lower Pelorus River",
    "Pelorus River at Kahikatea Flat" = "Upper Pelorus River",
    "Rai River at Rai Falls" = "Rai River",
    "Ronga River at Upstream Rai River" = "Ronga River",
    "Spring Creek at Wairau River Floodgates" = "Spring Creek",
    "Taylor River at Rail Bridge" = "Taylor River",
    "Tuamarina River at State Highway One" = "Tuamarina River",
    "Waihopai River at Craiglochart" = "Mid Waihopi River",
    "Waihopai River at SH63 Bridge" = "Upper Waihopi River",
    "Waima \\(Ure\\) River at SH1 Bridge" = "Waima River",
    "Wairau Diversion at Neals Road Bridge" = "Wairau Diversion",
    "Wairau River at Dip Flat" = "Upper Wairau River",
    "Wairau River at Tuamarina" = "Lower Wairau River",
    "Waitohi River at State Highway One" = "Waitohi River",
    "Wakamarina River at SH6" = "Wakamarina River"
  )
}

#### Backup of formulae

# func_SWQ_calcWQI <- function(tbl.in,guide.lines){
#   
#   boos.df <- tbl.in %>%
#     select(contains("_boo"))
#   
#   func_totaltests <- function(tests.vector){sum(!is.na(tests.vector))}
#   
#   total.tests <- boos.df %>% 
#     map_dbl(.,func_totaltests)
#   
#   F2 <- boos.df %>% 
#     map_dbl(.,sum,na.rm = "True")/sum(total.tests) *100
#   
#   F1 <- F2 %>% {if_else(.>0,1,0)}
#   F1 <- sum(F1)/length(F1) *100
#   
#   F2 <- sum(F2) ## Comment this line out if wanting parameter specific components of 
#   
#   #print(sum(total.tests))#debug
#   
#   func_SWQ_calcF3 <- function(vector.in,vector.boo,guide.lines){
#     true.rows=which(vector.boo == T, arr.ind = T)
#     vector.out <- double(length(vector.in))
#     for(i in true.rows){
#       if (vector.in[i]>guide.lines[2]){
#         vector.out[i] <- (vector.in[i]/guide.lines[2])-1
#       } 
#       else if (vector.in[i]<guide.lines[1]){
#         vector.out[i] <- (guide.lines[1]/vector.in[i])-1
#       }
#     }
#     vector.out <- sum(vector.out,na.rm = "True")
#     return(vector.out)
#   }
#   
#   excursion <- pmap_dbl(
#     list(select(tbl.in,one_of(colnames(guide.lines))),
#          select(tbl.in,one_of(paste0(colnames(guide.lines),"_boo"))),
#          guide.lines),
#     func_SWQ_calcF3
#   )
#   nse <- sum(excursion)/sum(total.tests)
#   
#   F3 <- nse/(0.01*nse+0.01)
#   
#   WQI <- 100-(sqrt((sum(F1))^2 + (sum(F2))^2 + F3^2)/1.732)
#   # return(WQI)
#   # 
#   return.tbl <- tibble(WQI,F1,F2,F3)
#   return(return.tbl)
# }


### Redundant formulae
# 
# func_SWQ_calcF1F2 <- function(data.in,guide.lines){
#   total_variables <- length(data.in[1,])
#   total_variables_thatfail <- integer(total_variables)
#   
#   total_elements <- as.integer(0)
#   total_elements_thatfail <- as.integer(0)
#   print(data.in)
#   # for(i in 1:total_variables){
#   #   fails <- sum(data.in[,i],na.rm = "True")
#   #   total_elements_thatfail <- total_elements_thatfail + fails
#   #   total_elements <- total_elements + sum(!is.na(data.in[,i]))
#   #   total_variables_thatfail[i] <- as.logical(fails)
#   # }
#   
#   F1 <- sum(total_variables_thatfail) / total_variables * 100
#   F2 <- total_elements_thatfail / total_elements * 100
#   
#   return(c(F1,F2))
# }