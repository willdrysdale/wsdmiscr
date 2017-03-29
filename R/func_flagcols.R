#' QC Flag a Column
#' 
#' Applies flags laid out in applyflags() to an individual column and formats dataframe accordingly
#' 
#' @param d dataframe containing data to be flagged
#' @param cols integer giving column number providing location of data to be flagged
#' 
#' @return dataframe with the one specified column flagged with formatting


############### Column Processing ################### 
flagcols = function(d,cols){
  if (missing(cols)) #If users has not specified paramaters
    stop("Please specifiy columns to be flagged") #stop
  n = names(d) #Store names of unflagged dataframe
  original_num_cols = length(n) #store number of columns in unfflagged dataframe
  flag_name = paste("qc_flag_",n[cols],sep = "") #form name of flag column
  n = c(n,flag_name) #add name of flag column to stored column names
  flagnum = original_num_cols+1 #set column for flags to be stored in to be 1 greater than the original number of columns
  d = applyflags(d,cols,flagnum) #flag data
  names(d) = n #renames columns including the the name of the flag column
  #Return  
  d 
}