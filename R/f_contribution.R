#' Footprint contribution
#' 
#' calculates the threshold above which all footprint contributions sum to greater than a target percentage contribution
#' 
#' @param foot_mat maxtrix of footprint contributions
#' @param percent_cont percentage (expressed as whole number) of target contribution
#' 
#' @author W S Drysdale
#' @export

f_cont = function(foot_mat,percent_cont = 80){
  #transform footprint matrix into a vector sorted decreasing
  foot_vec = foot_mat %>% as.vector %>% sort(decreasing = T)
  #calculate cumulative sum as new vector
  foot_cumsum = cumsum(foot_vec)
  #calculate target cumulative sum equivalent to selected percent contribution
  target = sum(foot_vec)*(percent_cont/100)
  #recover the threshold where all values greater than this sum to the percent contribution
  threshold = foot_vec[min(which(foot_cumsum >= target),na.rm = T)]
  #return
  threshold
}