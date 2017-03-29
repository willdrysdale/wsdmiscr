#' Pollution Colour
#' 
#' When supplied a list of colours and values to supply to pollutant arugments in openair
#' Return a vector of pollutant names
#' 
#' @param pol Vector of pollutant names
#' @param polcol list of colours and pollutants - where the colour is the item name and the pollutant is the list item
#' 
#' @export

pollution_colour = function(pol,polcol){
  k = paste("\\b",pol[1],"\\b",sep = "")
  x = grep(k,polcol)
  y = names(polcol[x])
  if (length(pol) >1){
    for (i in 2:length(pol)){
      k = paste("\\b",pol[i],"\\b",sep = "")
      x = grep(k,polcol)
      y = c(y,names(polcol[x]))
    }
  }
  #Return
  y
}