#' Group By Bin
#'
#' Takes a parameter and summarises by a bin column, then join this column onto the timeseries
#' can be used to bin secondary parameters for things like geom_boxplot, where the fill aesthetic must be pre binned
#' 
#' 
#' @param d dataframe containg col and group_col
#' @param col name of column to group
#' @param group_col name of column to group by i.e. the bins
#' @param .f function to summarise by default mean
#' @param ... additional parameters for .f
#' 
#' @author W. S. Drysdale and F. A. Squires
#' 
#' @export

group_by_bin = function(d, col, group_col, .f = mean, ...){
  new_d = d[,c(group_col,col)] %>% 
    group_by_(group_col) %>% 
    summarise_all(.f, ...)
  
  names(new_d)[2] = paste0(col,"_",group_col)
  
  left_join(d,new_d,by = group_col)
  
}
