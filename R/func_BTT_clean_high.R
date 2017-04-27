#' BTT Clean High
#' 
#' Removes NO,No2 and NOx values greater than 500 ppb and o3 greater than 300 ppb
#' Flags these as 5 - manually removed
#' 
#' @param d flagged BT tower data
#' @export

BTT_clean_high = function(d){
  
  d$qc_flag_no[d$no > 500] = 5
  d$qc_flag_no2[d$no > 500] = 5
  d$qc_flag_nox[d$no > 500] = 5
  d$qc_flag_no[d$no > 500] = 5
  d$qc_flag_no[d$o3 > 300] = 5
  
  d$no[d$no > 500] = NA
  d$no2[d$no > 500] = NA
  d$nox[d$no > 500] = NA
  d$no[d$no > 500] = NA
  d$no[d$o3 > 300] = NA
  
  #return
  d
}