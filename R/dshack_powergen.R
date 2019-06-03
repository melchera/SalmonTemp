#' Power Generation at Dworshack Dam
#'
#' This function computes the maximum possible power generation
#' at Dworshack dam given the head and flow rate through turbines (reported daily from Army Corps)
#’ 

#' @param rho Density of water (kg/m3) Default is 1000
#' @param g Acceleration due to gravity (m/sec2) Default is 9.8
#' @param Kefficiency Turbine Efficiency (0-1) Default is 0.9 for Francis Turbine
#' @param head net head of water in reservoir (m)
#' @param flow flow rate (m3/sec) 
#' @author Savannah Tjaden
#' @return Maximum possible power generation (W)


max_power = function(head, flow, rho=1000, g=9.8, Keff=0.9) {
  
  # make sure inputs are positive
  if (head < 0) return(NA)
  if (flow < 0) return(NA)
  if (rho < 0) return(NA)
  
  # calculate power
  result = rho * flow * head * g * Keff
   
  
  
  return(result)
}

