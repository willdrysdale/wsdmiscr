#' Time Variation Intereaction Level 2
#' 
#' Allows the user to specify a list of pollutants to be plotted on the same timeVariation plot
#' To Skip this or to break out of the plotting cycle, the user should answer "n" to the intitial question
#' 
#' @param d dataframe being processed by massPlot
#' @param pol pollutants within dataset 

timeVarselection = function(d,pol,s){
  n = names(d)
  i = readline(prompt = paste("Plot Selection of Pollutants? Confirm [y/n]: "))
  if (i == "y" | i == "n") {  
    if (i == "y"){
      new_pol = selectpol(n)
      timeVariation(d,pollutant = new_pol)
      if (s == T){
        pdf(file = NULL)
        se = timeVariation(d,pollutant = new_pol,key = T, type = "season")
        dev.off()
        plot(se$plot$hour)
      }
      timeVarselection(d,pol,s)
    }
    if (i == "n")
      print("End timeVariation")
  }else
    timeVarselection(d,pol,s)
}