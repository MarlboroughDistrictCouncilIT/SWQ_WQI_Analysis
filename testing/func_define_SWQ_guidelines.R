func_SWQ_guidelines <- function(){
LowThresholds = c(
  SINitrogen = 1E-4, #mg/L as N
  Phosphorus = 1E-4, #mg/L
  Oxygen = 70, #%
  Turbidity = 1E-4, #NTU
  Ecoli = 1E-4, #units/100mL
  Temp = 1E-4,#degC
  pH = 6.7,
  Ammonia_Summer=1E-4, #mg/L as N
  Ammonia_Winter=1E-4, #mg/L as N
  Nitrate=1E-4
)
HighThresholds = c(
  SINitrogen = 0.165, #mg/L as N
  Phosphorus = 0.015, #mg/L
  Oxygen = 100, #%
  Turbidity = 5.6, #NTU
  Ecoli = 550, #units/100mL
  Temp = 21.5, #degC
  pH = 7.8,
  Ammonia_Summer=0.2, #mg/L as N
  Ammonia_Winter=0.75, #mg/L as N
  Nitrate=2.4
)
cnames <- c("SINitrogen", "Phosphorus", "Oxygen", "Turbidity", "Ecoli", "Temp", "pH", "Ammonia_Summer","Ammonia_Winter","Nitrate")
rnames <- c("Low","High")           
guidelineMatrix <- matrix(c(LowThresholds,HighThresholds), nrow = 2, byrow = T, dimnames = list (rnames, cnames))
return(guidelineMatrix)
}
