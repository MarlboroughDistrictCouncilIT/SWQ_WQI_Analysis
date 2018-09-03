func_SWQ_calcF1 <- function(data.in){
  totalfails <- as.integer(0)
  totalvariables <- as.integer(0)
  
  for(i in 1:length(data.in[1,])){
      totalfails <- totalfails + sum(data.in[,i],na.rm = "True")    
      totalvariables <- totalvariables + sum(!is.na(data.in[,i])) # Works for individual vector
  }  
      
  #totalvariables <- data.in
  F1 <- totalfails/totalvariables
  # F1 <- totalfails/totalvariables
  # totalvariables <- sum(!is.na(F1$Oxygen_boo)) # Works for individual vector
  # totalfails <- sum(F1$Oxygen_boo,na.rm = "True")
  return(c(totalfails,totalvariables,F1))
}
