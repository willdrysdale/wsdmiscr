#' Process Aircraft Data
#' 
#' Reads in an AQD NOx aircraft file and runs aircraft_cal_flags, prodcuing quicklook and full outputs
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


process.aircraft = function(fn,dir_out = "",flight_id = "XXXX"){
  d = read.aircraft(fn)
  d_processed = aircraft_cal_flags(d)
  d_final = d_processed[,c(1,54,55,56)]
  write.csv(d_processed,paste(dir_out,flight_id,"_processed.csv",sep = ""),row.names = F,na = "")
  write.csv(d_final,paste(dir_out,flight_id,"_quick.csv",sep = ""),row.names = F,na = "")
  out = list(d_processed,d_final)
  names(out) = c(paste(flight_id,"_processed",sep = ""),paste(flight_id,"_quick",sep = ""))
  out
}