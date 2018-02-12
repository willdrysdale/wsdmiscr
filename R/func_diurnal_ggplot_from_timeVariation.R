#' Diurnal ggplot from timeVariation
#' 
#' Uses the day.hour output of openair::timeVariation and returns a ggplot version of the plot
#' 
#' @param d day.hour dataframe from openair::timeVariation
#' 
#' @return data frame ready for ggplot
#' 
#' @author W. S. Drysdale
#' 
#' @export

diurnal_ggplot_from_timeVariation = function(d){
  d$day_frac = d$hour/23
  d$day_num = NA
  
  days = levels(d$wkday)
  for (i in 1:7)
    d$day_num[d$wkday == days[i]] = i
  
  d$index = d$day_frac + d$day_num
  if(class(d$variable) == "factor")
    d$variable = as.character(d$variable)
  

  #Return new dataframe
  d
}



            