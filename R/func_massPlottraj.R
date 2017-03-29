#' Plot Back Trajectories
#' 
#' Incomplete and currently not run when calling massPlot()
#' 
#' @param backTraj A backTrajectory file, pre merged with concentration data
#' @param pol pollutants within dataset 
#' @param start string in format "dd/mm/yyyy" to be passed to selectBydate()
#' @param end string in format "dd/mm/yyyy" to be passed to selectBydate()
#' @param season If true, the time series is of a length suitable for seasonality comparison  


massPlottraj = function(backTraj,pol,start,end,s){

  n = names(backTraj)

  j = readline(prompt = paste("Plot Each Pollutant? Confirm [y/n]: "))
  if (j == "y" | j == "n"){
    if (j == "y"){
      if (s == T){
        for(i in 1:length(pol)){
          trajPlot(selectByDate(backTraj,start = start,end = end),pol = pol[i],col = "increment")
          trajPlot(selectByDate(backTraj,start = start,end = end),pol = pol[i],type = "season",
                   layout = c(2,2),col = "increment")
          trajLevel(selectByDate(backTraj,start = start,end = end),pol = pol[i],
                    statistic = "CWT",smooth = T,col = "increment")
          trajLevel(selectByDate(backTraj,start = start,end = end),pol = pol[i],
                    statistic = "CWT",smooth = T,type = "season",layout = c(2,2),col = "increment")
          print(paste(i,"/",length(pol)," pollutants plotted",sep = ""))
        }
        print("End Backtrajectories")
      }else{
        for(i in 1:length(pol)){  
          trajPlot(selectByDate(backTraj,start = start,end = end),pol = pol[i],col = "increment")
          trajLevel(selectByDate(backTraj,start = start,end = end),pol = pol[i],
                    statistic = "CWT",smooth = T,col = "increment")
          print(paste(i,"/",length(pol)," pollutants plotted",sep = ""))
        }
        print("End Backtrajectories")
      }
    }else{
      if (j == "n"){
        new_pol = selectpol(n)
        if (s == T){
          for(i in 1:length(new_pol)){
            trajPlot(selectByDate(backTraj,start = start,end = end),pol = new_pol[i],col = "increment")
            trajPlot(selectByDate(backTraj,start = start,end = end),pol = new_pol[i],type = "season",
                     layout = c(2,2),col = "increment")
            trajLevel(selectByDate(backTraj,start = start,end = end),pol = new_pol[i],
                      statistic = "CWT",smooth = T,col = "increment")
            trajLevel(selectByDate(backTraj,start = start,end = end),pol = new_pol[i],
                      statistic = "CWT",smooth = T,type = "season",layout = c(2,2),col = "increment")
            print(paste(i,"/",length(pol)," pollutants plotted",sep = ""))
          }
          print("End Backtrajectories")
        }else{
          for(i in 1:length(pol)){  
            trajPlot(selectByDate(backTraj,start = start,end = end),pol = new_pol[i],col = "increment")
            trajLevel(selectByDate(backTraj,start = start,end = end),pol = new_pol[i],
                      statistic = "CWT",smooth = T,col = "increment")
            print(paste(i,"/",length(pol)," pollutants plotted",sep = ""))
          }
          print("End Backtrajectories")
        }
      }
    }
  }else
    massPlottraj(backTraj,pol,start,end,s)
}