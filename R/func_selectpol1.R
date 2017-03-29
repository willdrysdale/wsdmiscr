#' Select Pollutant Sub step 1
#' 
#' Allows for error catching whilst selecting pollutants
#' 
#' @param n vector of column headers

spstep1 = function(n){
  
  x = readline(prompt = paste("Pollutant 1: "))
  if (!x %in% n){
    print("No pollutant or column header with that name found")
    spstep1(n)
  }else
    return(x)
}