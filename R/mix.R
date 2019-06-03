#' mix
#'
#' This model computes the output streamflow and temperature from two different rivers mixing, each with their own streamflow and temperature. 
#' @param Q1  streamflow of the recieving water in cubic feet per second (cfs)
#' @param Q2  streamflow of the tributary water in cubic feet per second (cfs)
#' @param T1 water temperature of the recieving water in degrees Celcius
#' @param T2 water temperature of the tributary water in degrees Celcius
#' @return the function returns the streamflow (cfs) and temperature (degrees C) of the combined rivers 
#' @example mix()
#' 
#' 


mix = function(Q1, Q2, T1, T2) {
  
  mix_temp = (T1*(Q1/(Q1+Q2))) + (T2*(Q2/(Q1+Q2)))
  mix_flow = Q1 + Q2
  
  return(list(mix_temp, mix_flow))
  
}
