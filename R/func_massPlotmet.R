#' Plot Met
#' 
#' Creates the plots that require Met data, windrose, pollutionrose and polarplot
#' Where relevaent these plots are created for each species specified in massPlot(pol)
#' 
#' @param d dataframe being processed by massPlot
#' @param pol pollutants within dataset 

massPlotmet = function(d,pol,s){
  windRose(d,paddle = F)
  if (s == T)
    windRose(d,paddle = F,type = "season",layout = c(2,2))
  for (i in 1:length(pol)){
    polarPlot(d, pollutant = pol[i])
    if (s == T)
      polarPlot(d, pollutant = pol[i],type = "season",layout = c(2,2))
    pollutionRose(d, pollutant = pol[i])
    if (s == T)
      pollutionRose(d, pollutant = pol[i],type = "season",layout = c(2,2))
    print(paste(i,"/",length(pol)," pollutants plotted",sep = ""))
  }

}