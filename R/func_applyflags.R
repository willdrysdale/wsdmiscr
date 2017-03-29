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

applyflags = function(d,cols,flagnum){
  d[,flagnum] = 1 #Flag all data as Good
  d[,flagnum][d[,cols] < 1] = 2 #All data less than 1 ppb flagged as below LoD
  d[,flagnum][d[,cols] < 0] = 3 #All data less than 0 ppb flagged as in zero range (thus bounding LoD flag)
  d[,flagnum][d[,cols] < -1] = 4#All data less than -1 ppb flagged as erroneous (thus bounding zero range)
  d[,flagnum][is.na(d[,cols])] = 0#All data that is NA flagged
  #Return
  d
}