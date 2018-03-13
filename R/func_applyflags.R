#' Apply Flags
#' 
#' Adds a column with QC flags based on the paramaters listed:
#' Flag Concentration data:
#' 0 good data - the default unless specific paramaters are met
#' 1 data below limit of detection 0-LOD
#' 3 NA 
#' 
#' @param d dataframe containing data to be flagged
#' @param cols integer value of column containg data to be flagged
#' @param flagnum integer value of column where flags are to be stored
#' 
#' @return dataframe with QC flag column added

applyflags = function(d,cols,flagnum,LOD){
  d[,flagnum] = 0 #Flag all data as Good
  d[,flagnum][d[,cols] < LOD] = 1 #All data less than LOD ppb flagged as below LoD
  d[,flagnum][is.na(d[,cols])] = 3#All data that is NA flagged
  #Return
  d
}