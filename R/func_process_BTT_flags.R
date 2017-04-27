#' Process BTT Flags
#'
#' Applies the rules that each flag specifies to the data set
#' 0 data not used - Used when value is NA
#' 1 good data - the default unless specific paramaters are met
#' 2 data below limit of detection 0-1 ppb
#' 3 data less than zero but within zero range -1-0 ppb
#' 4 data less than -1 ppb, to be considered erroneous and should be converted to NA manually
#' 
#' @param d BTT flagged data set
#' @export

process_BTT_flags = function(d,cols){
  for (i in cols){
    pol_name = names(d)[i]
    flag_name = paste("qc_flag_",pol_name,sep = "")
    
    d[,pol_name][d[,flag_name] == 2] = 0.5
    d[,pol_name][d[,flag_name] == 3] = 0
    d[,pol_name][d[,flag_name] == 4] = NA
  }
  return(d)
}