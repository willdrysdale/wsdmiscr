#' Plot Time Plots
#' 
#' Forms a imte plot for each species specified in massPlot(pol)
#' 
#' @param d dataframe being processed by massPlot
#' @param pol pollutants within dataset 


massPlottimeseries = function(d,pol){
  for (i in 1:length(pol)){
    timePlot(d, pollutant = pol[i])
  }
}