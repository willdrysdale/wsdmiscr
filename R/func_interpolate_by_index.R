##' @name interpolate_by_index
##' 
##' @title interpolate_by_index
##' 
##' @title interpolate_by_index
##' @description interpolates between a coloumn between a defined start and end index
##' @param d dataframe to interpolate over
##' @param index_start value of index at begining of index. index must be unique. length must equal \code{length(interp_col)}
##' @param index_end end of index value
##' @param interp_col names of column, or vector of names to interpolate over
##' @param index name of index column, default "date"
##' @param f function to use for interpolation, default \code{zoo::na.approx}
##' @param extrap logical. when true, na.locf is applied to the whole interpolation column after the initial interpolation, expand the first and last values to the whole column
##' @author W. S. Drysdale
##' 
##' @export


interpolate_by_index = function(df,index_start,index_end,interp_col,index = "date", f = zoo::na.approx,extend = F){
  
  if(length(index_start) == length(index_end)){
    if(length(index_start) == length(interp_col)){
      for(i in 1:length(index_start)){
        df = interpolate_by_index_wrk(df,index_start[i],index_end[i],interp_col[i],index = "date", f = zoo::na.approx)
      }
      if(extend){
        for(col in paste0(interp_col,"_interp")){
          df[,col] = zoo::na.locf(df[,col],fromLast = T)
        }
      }
      #Return
      df
    }
  }else
    stop("index_start, index_end and interp_col must be of equal length")
}
##' @rdname interpolate_by_index_wrk
##' @title interpolate_by_index
##' @description interpolates between a coloumn between a defined start and end index
##' @param d dataframe to interpolate over
##' @param index_start value of index at begining of index. index must be unique. length must equal \code{length(interp_col)}
##' @param index_end end of index value
##' @param interp_col names of column, or vector of names to interpolate over
##' @param index name of index column, default "date"
##' @param f function to use for interpolation, default \code{zoo::na.approx}
##' @export

interpolate_by_index_wrk = function(d,index_start,index_end,interp_col,index = "date", f = zoo::na.approx){
  new_col = paste0(interp_col,"_interp")
  
  d[,new_col] = NA
  d[,new_col][d[,index] == index_start] = d[,interp_col][d[,index] == index_start]
  d[,new_col][d[,index] == index_end] = d[,interp_col][d[,index] == index_end]
  d[,new_col][which(d[,index] == index_start):which(d[,index] == index_end)] %<>% f()
  
  d
}
