#' air_warming
#'
#' This function calculates the warming of a river at a run-of-the-river dam from a non-stratifying reservoir with equal inflow and outflow. Default physical parameters are for Lewiston, Idaho.
#' @param Tw Temperature of water (degrees Celsius)
#' @param Ta Temperature of air (degrees Celsius)
#' @param flow Inflow of water into the reservoir (cubic feet per second)
#' @example air_warming(17, 22, 30000)
#' @return dTw, change in water temperature (degrees Celsius)

air_warming = function(Tw, Ta, flow) {
  
  #------------------Celsius to Kelvin-------------
  Tw = Tw + 273.15 #convert from C to K
  Ta = Ta +273.15 #convert from C to K
  
  #------------------Heat Flux Equation----------------------  
  ### Qsw - net shortwave radiation ###
  s = 1362 #W/m^2 - solar radiation constant
  albedo = 0.06 
  
  Qsw = (s/4) * (1-albedo) #W/m^2
  Qsw = Qsw * 86400 / 1000 #convertt to kJ/m^2*day
  
  ### Qlw - net long wave radiation ###
  C = 0.1 #assumes average 10% cloud cover
  
  Qlw = (5.23*10^-8) * Tw^4 - (5.18*10^-13) * Ta^6 * (1 + 0.2 * C^2) #W/m^2
  Qlw = Qlw * 86400 / 1000 #convert to kJ/m^2*day
  
  ### Hl - Latent heat of evaporation ###
  rho_a = 1.2135 #kg/m^3 - density of air
  Lw = 2260 #kJ/kg - latent heat of evaporation
  Cl = 0.0015 #exchange coefficient
  U10 = 4 #m/s - wind speed at 10 meters above surface
  qs = 0.7 #saturation humidity at the water surface temperature
  qa = 0.59 #relative humidity at 10 meters above surface
  
  Hl = rho_a * Lw * Cl * U10 *(qs - qa) #kJ/m^2*s
  Hl = Hl * 86400 #convert to kJ/m^2*day
  
  ### Hs - Sensible heat flux ###
  Cs = Cl #exchange coefficient
  Cp = 4.186 #kJ/kg C - heat capacity of water
  
  Hs = rho_a * Cp * Cs * U10 * (Tw - Ta) #kJ/m^2*s
  Hs = Hs * 86400 #convert to kJ/m^2*day
  ### H - Heat flux ###
  H = Qsw + Qlw + Hl + Hs #kJ/m^2*day
  
  #-----------------Temperature Change Equation--------------
  #Stable parameters
  rho_w = 997 #kg/m^3 - density of water
  V = 54300000 #m^3 - volume of reservoir
  A = 3000000 #m^2 - surface area of reservoir
  
  ### residence time ###
  flow = flow * 0.028317 * 60 * 60 *24 #convert from ft^3/s to m^3/day
  residence_time = V / flow #residence time of water in reservoir based on average flow rate in, assuming equal flowrate out
  time_step_of_eq = 1 #day
  
  dt = residence_time / time_step_of_eq
  
  dTw = ((A * H) / (rho_w * Cp * V)) * dt
  
  return(list(dTw))
}
  

