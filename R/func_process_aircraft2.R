#' Process Aircraft Data2
#' 
#' Reads in an AQD NOx aircraft file and runs aircraft_cal_flags, prodcuing quicklook and full outputs
#' Used on Files after flight C019
#' 
#' 
#' @param fn file to read
#' @param dir_out output directory
#' @param flight_id ID of flight
#' 
#' @return Saves two csvs into the output directory "processed" full output and "quicklook" only containing
#' time and concetrations. returns a list containg these two dataframes
#' 
#' @author Will S. Drysdale
#' @author Freya A. Squires
#' 
#' @export


process.aircraft2 = function(fn,dir_out = "",flight_id = "XXXX"){
  d = read.aircraft(fn)
  d_processed = aircraft_cal_flags(d)
  d_quick = d_processed[,c(1,65,66,67)]
  d_final = aircraft_clean_negatives(d_quick)
  write.csv(d_processed,paste(dir_out,flight_id,"_processed.csv",sep = ""),row.names = F,na = "")
  write.csv(d_quick,paste(dir_out,flight_id,"_quick.csv",sep = ""),row.names = F,na = "")
  write.csv(d_final,paste(dir_out,flight_id,"_final.csv",sep = ""),row.names = F,na = "")
  out = list(d_processed,d_quick,d_final)
  names(out) = c(paste(flight_id,"_processed",sep = ""),
                 paste(flight_id,"_quick",sep = ""),
                 paste(flight_id,"_final",sep = "")
                 )
  out
}