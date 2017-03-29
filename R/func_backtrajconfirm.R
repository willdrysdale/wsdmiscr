#' Back Trajectory Confirm
#' 
#' Confirms with the user to continue plotting Back Trajectories, as these can take time
#' 
#' @param backTraj A backTrajectory file, pre merged with concentration data
#' @param pol pollutants within dataset 
#' @param start string in format "dd/mm/yyyy" to be passed to selectBydate()
#' @param end string in format "dd/mm/yyyy" to be passed to selectBydate()

backTrajconfirm = function(backTraj,pol,start,end,s){

  j = readline(prompt = cat("Warning BackTrajectories take a long time to plot,\n especially over large time periods. Confirm [y/n]?"))
  if (j == "y" | j == "n"){
    if (j == "y"){
     print("Begining BackTrajectories")
     massPlottraj(backTraj,pol,start,end,s)
    }else{
      if (j == "n")
        print("Back Trajectories Cancelled")
      }
  }else
    backTrajconfirm(backTraj,pol,start,end,s)
}