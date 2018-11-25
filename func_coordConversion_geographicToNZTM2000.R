coord_geoToNZTM <- function(lat=lat,lon=lon){
  
  phi <- lat * pi/180 #radians
  lambda <- lon * pi/180 #radians
  
  ## define projection Parameters 
  pP <- data.frame(a = 6378137,
                   f = 1/298.257222101, 
                   phi_0 = 0 * pi/180, #radians
                   lambda_0 = 173 * pi/180, #radians
                   N_0 = 10E6,
                   E_0 = 1.6E6,
                   k_0 = 0.9996
                   )
  
  ## calc Projection constants
  b <-  pP$a*(1-pP$f)
  eSquare <- 2*pP$f - pP$f^2
  A_0 <- 1 - (eSquare/4) - (3*eSquare^2/64) - (5*eSquare^3/256)
  A_2 <- 3/8 * (eSquare + (eSquare^2/4) + (15*eSquare^3/128))
  A_4 <- 15/256 * (eSquare^2 + (3*eSquare^3/4))
  A_6 <- 35*eSquare^3/3072
  
  print(c(A_0, A_2, A_4, A_6))
  
  calc_m <- function(phi){
     m <- pP$a*((A_0*phi) - A_2*sin(2*phi) + A_4*sin(4*phi) - A_6*sin(6*phi))
     return(m)
  }
  
  ## Geographic to Tranverse Mercator
  ## First parameters at computation point:
  rho <- (pP$a*(1 - eSquare))/(1 - eSquare *sin(phi)^2 )^3/2
  neu <- pP$a/sqrt(1 - eSquare*sin(phi)^2)
  psi <- neu / rho
  t <- tan(phi)
  omega <- lambda - pP$lambda_0
  
  ## Second Calc Northing
  N_T1 <- omega^2/2 * neu * sin(phi) * cos(phi)
  N_T2 <- omega^4/24 * neu * sin(phi) * cos(phi)^3 * (4*psi^2 + psi - t^2)
  N_T3 <- omega^6/720 * neu * sin(phi) * cos(phi)^5 * (8*psi^4 * (11 - 24*t^2) - 28*psi^3 * (1-6*t^2) + psi^2*(1-32*t^2) - psi*(2*t^2) + t^4) 
  N_T4 <- omega^8/40320 * neu * sin(phi) * cos(phi)^7 * (1385 - 3111*t^2 + 543*t^4 - t^6)
  
  Northing = pP$N_0 + pP$k_0*(calc_m(phi) - calc_m(pP$phi_0) + N_T1 + N_T2 + N_T3 + N_T4)
  
  ## Third Calc Easting
  E_T1 <- omega^2/6* cos(phi)^2 * (psi - t^2)
  E_T2 <- omega^4/120 * cos(phi)^4 * ( (4*psi^3*(1-6*t^2)) + psi^2*(1+8*t^2) - psi*2*t^2 + t^4 )
  E_T3 <- omega^6/5040 * cos(phi)^6 *(61 - 479*t^2 + 179*t^4 - t^6)
  
  Easting <- pP$E_0 + pP$k_0 * neu * omega * cos(phi) *(1 + E_T1 + E_T2 + E_T3)
  
  return(c(Easting, Northing))
}
  