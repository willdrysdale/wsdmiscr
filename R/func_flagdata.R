#' Flag Data
#'
#' Applies Flags to data as specified in applyflags() ti multiple columns
#'
#' @param d dataframe containing data to be flagged
#' @param cols vector argument giving column numbers providing location of data to be flagged
#' 
#' 
#' @return dataframe containg flagged data
#' 
#' @export

flagdata = function(d,cols,LOD){
  for (i in 1:length(cols)){ #for columns specified (including one column)
    d = flagcols(d,cols[i],LOD) #perform flag process for each
  }
  #Return
  d
}