#' Summary Interaction
#' 
#' Collects information about the data set with respect to passing values to summaryPlot()
#' 
#' @param d dataframe being processed by massPlot

massplotsummary = function(d){
  i = readline(prompt = paste("Period of data less than a year? [y/n]: "))
  if (i == "y" | i == "n") {  
    if (i == "y"){
      summaryPlot(d,period = "months")
    }
    if (i == "n")
      summaryPlot(d)
  }else
    massplotsummary(d)
}
  