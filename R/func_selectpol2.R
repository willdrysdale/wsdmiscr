#' Select Pollutant Sub step 2
#' 
#' Allows for error catching whilst selecting multiple pollutants
#' 
#' @param n vector of column headers
#' @param t number of pollutants to plot
#' @param x first pollutant selected in step 1
#' @param i count variable from loop in selectpol

spstep2 = function (n,t,x,i){
  y = readline(prompt = paste("Pollutant ",i,": ",sep = ""))
  if (y %in% n){
    xa = c(x,y)
    #Return
    return(xa)
  }else{
    print("No pollutant or column header with that name found")
    spstep2(n,t,x,i)
  }
}