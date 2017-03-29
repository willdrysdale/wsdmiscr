#' Mass Plot
#' 
#' Upon being given a dataset - this function produces a series of Openair plots using the avalible data.
#' These include timeVariation, wind and pollution rose and summary plots. 
#' Output into one PDF. Data should be supplied to comply with Openair formatting, with the time stamp in a column labled "date", in a format parseable by lubridates ymd_hms() function
#' 
#' @param d dataframe being processed
#' @param pol pollutants within dataset supplied as vector
#' @param fn the name and/or path where the file shall be saved
#' @param backTraj A backTrajectory file, pre merged with concentration data
#' @param start string in format "dd/mm/yyyy" to be passed to selectBydate()
#' @param end string in format "dd/mm/yyyy" to be passed to selectBydate() 
#' @param s TRUE or FALSE. Is the data suitable for creating seasonal plots?
#' 
#' 
#' 
#' @return A PDF containing all of the plots specified during the running of the function
#'
#' @export


massPlot = function(d,pol,fn,backTraj,start,end,s){
  d$date = ymd_hms(d$date)
  
  pdf(file = fn, width = 15)
  #----Met independant plots---#
  print("Plotting Summary")
  massplotsummary(d)
  
  print("Plotting timePlot")
  massPlottimeseries(d,pol)
  
  print("Begining timeVariation")
  massPlottimeVar(d,pol,s)
  #massPlottraj(d,pol)
  
  #----Met Dependant plots---#
  print("Plotting windRose, polarPlot, pollutionRose")
  if(("ws" %in% colnames(d))&("wd" %in% colnames(d)))
    massPlotmet(d,pol,s)
  else
    print("Check Met Data - could not find ws or wd")
  
  #---Back Trajectory by pollutant---#
  if(!missing(backTraj)){
    if (missing(start) | missing(end))
      print("No start or end specified")
    else
      backTrajconfirm(backTraj,pol,start,end,s)
  }
  dev.off()
}