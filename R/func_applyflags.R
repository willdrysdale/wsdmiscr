#' Apply Flags
#' 
#' Adds a column with QC flags based on the paramaters listed:
#' Flag Concentration data:
#' 0 data not used - Used when value is NA
#' 1 good data - the default unless specific paramaters are met
#' 2 data below limit of detection 0-1 ppb
#' 3 data less than zero but within zero range -1-0 ppb
#' 4 data less than -1 ppb, to be considered erroneous and should be converted to NA manually
#' 
#' @param d dataframe containing data to be flagged
#' @param cols integer value of column containg data to be flagged
#' @param flagnum integer value of column where flags are to be stored
#' 
#' @return dataframe with QC flag column added

applyflags = function(d,cols,flagnum,LOD){
  d[,flagnum] = 0 #Flag all data as Good
  d[,flagnum][d[,cols] < LOD] = 1 #All data less than 1 ppb flagged as below LoD
  d[,flagnum][is.na(d[,cols])] = 3#All data that is NA flagged
  #Return
  d
}