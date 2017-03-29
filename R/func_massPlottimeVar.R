#' Time Variation Intereaction Level 1
#' 
#' Questions the user whether all the pollutants originally passed to massPlot() should be plotted on the same timeVariation() graph.
#' Regardless of whether a plot is created in this step,timeVarselection() is called to create plots of subsets of pollutants
#'
#' @param d dataframe being processed by massPlot
#' @param pol pollutants within dataset 



massPlottimeVar = function(d,pol,s){
  i = readline(prompt = paste("Plot all pollutants? Confirm [y/n]: "))
  if (i == "y" | i == "n"){
    if (i == "y"){
      timeVariation(d,pollutant = pol)
      if (s == T){
        pdf(file = NULL)
        se = timeVariation(d,pollutant = pol,key = T, type = "season")
        dev.off()
        plot(se$plot$hour)
      }
      timeVarselection(d,pol,s)
    }else{
      if (i == "n"){
        timeVarselection(d,pol,s)
      }
    }
  }else
    massPlottimeVar(d,pol,s)
}