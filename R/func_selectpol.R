#' Select Pollutant
#' 
#' Collects pollutant information to plot for timeVar
#' 
#' @param n vector of column headers in dataframe - for error checking
#' 
#' @return vector of pollutant names

selectpol = function(n){
  t = as.numeric(readline(prompt = paste("How many pollutants to plot?: ")))
  if (!grepl("^[0-9]+$",t) | !t >= 1){
    print("Please enter an integer greater than 0")
    selectpol(n)
  }
  
  x = spstep1(n)
  
  if (t > 1)
    for (i in 2:t)
      x = spstep2(n,t,x,i)
  #Return
  x
}
