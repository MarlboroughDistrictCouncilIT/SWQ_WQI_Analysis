func_coord_NZTMToGeo <- function(northing, easting, output = "latlon"){
  ## define projection Parameters 
  pP <- data.frame(a = 6378137,
                   f = 1/298.257222101, 
                   phi_0 = 0 * pi/180, #radians
                   lambda_0 = 173 * pi/180, #radians
                   N_0 = 10E6, #+425,
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
  
  calc_m <- function(phi){
     m <- pP$a*((A_0*phi) - A_2*sin(2*phi) + A_4*sin(4*phi) - A_6*sin(6*phi))
     return(m)
  }
  
  ## Transverse Mercator to Geographic
  ## First parameters at computation point:
  N_p <- northing - pP$N_0
  m_p <- calc_m(pP$phi_0) + N_p/pP$k_0
  n <- (pP$a - b)/(pP$a + b)
  G <- pP$a * (1-n) * (1-n^2) * (1 + 9*n^2/4 + 225*n^4/64) * (pi/180) #### Bif this radian conversion if necessary
  sig <- (m_p * pi)/(180*G) 
  phi_p <- sig + (3*n/2 - 27*n^3/32)*sin(2*sig) + (21*n^2/16 - 55*n^4/32)*sin(4*sig) + (151 * n^3/96)*sin(6*sig) + (1097*n^4/512)*sin(8*sig)
  
  rho_p <- (pP$a*(1-eSquare))/(1-eSquare*sin(phi_p)^2)^3/2
  neu_p <- pP$a/sqrt(1-eSquare*sin(phi_p)^2)
  psi_p <- neu_p/rho_p
  t_p <- tan(phi_p)
  E_p <- easting - pP$E_0
  chi <- E_p/(pP$k_0 * neu_p)
  
  ## Second Calc latitude
  lat_T1 <- (t_p * E_p * chi)/(pP$k_0 * rho_p * 2)
  lat_T2 <- (t_p * E_p * chi^3)/(pP$k_0 * rho_p * 24) * (-4*psi_p^2 + 9*psi_p*(1-t_p^2) + 12*t_p^2)
  lat_T3 <- (t_p * E_p * chi^5)/(pP$k_0 * rho_p * 720) * (8*psi_p^4*(11-24*t_p^2) - 12*psi_p^3*(21-71*t_p^2) + 15*psi_p^2*(15-98*t_p^2+15*t_p^4) + 180*psi_p*(5*t_p^2-3*t_p^4) + 360*t_p^4)
  lat_T4 <- (t_p * E_p * chi^7)/(pP$k_0 * rho_p * 40320)*(1385 + 3633*t_p^2 + 4095*t_p^4 + 1575*t_p^6)
  
  lat <- phi_p - lat_T1 +lat_T2 - lat_T3 + lat_T4
  lat <- lat * 180/pi ## convert radian to degrees
  
  ## Third Calc longditude
  lon_T1 <- chi * 1/ (cos(phi_p))
  lon_T2 <- chi^3 * 1/(6*cos(phi_p)) * (psi_p + 2*t_p^2)
  lon_T3 <- chi^5 * 1/(120*cos(phi_p)) * (-4*psi_p^3*(1-6*t_p^2) + psi_p^2*(9-68*t_p^2) + 72*psi_p*t_p^2 * 24*t_p^4)
  lon_T4 <- chi^7 * 1/(5040*cos(phi_p)) * (61 + 662*t_p^2 + 1320*t_p^4 + 720*t_p^6)
  
  lon <- pP$lambda_0 + lon_T1 - lon_T2 + lon_T3 - lon_T4
  lon <- lon * 180/pi ## convert radians to degrees
  
  if(output == "lat"){
    return(lat)
  } else if (output == "lon"){
    return(lon)
  } else {
    return(c(lat,lon))
  }
}
  