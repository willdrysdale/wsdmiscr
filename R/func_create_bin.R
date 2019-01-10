#' Create Bin
#' 
#' Create a column of data bins based on breaks supplied by user. upper end of break is used as bin
#' 
#' @param d dataframe
#' @param col column of data to assign breaks to
#' @param breaks sequence of breaks to used for bins
#' 
#' @author W. S. Drysdale and F. A. Squires
#' 
#' @export

create_bin = function(d,col,breaks){
  new_col = d[col]
  new_col = new_col[,1]
  for(i in 1:length(breaks)){
    if(i == 1){
      new_col[d[col] >= min(d[col],na.rm = T) & d[col] <= breaks[i]] = breaks[i]
    }
    
    if(i == length(breaks)){
      new_col[d[col] <= max(d[col],na.rm = T) & d[col] >= breaks[i]] = breaks[i]
    }
    
    if(i != 1 | i != length(breaks)){
      new_col[d[col] > breaks[i] & d[col] <= breaks[i+1]] = breaks[i]
    }
    
  }
  new_col
}


