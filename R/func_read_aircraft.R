#' Read Aircraft CSV
#' 
#' Reads in an aircraft CSV and creates the UNIX_TS column used by aircraft_cal_flag
#' 
#' @param fn location of CSV
#' 
#' @return datafram of CSV with UNIX_TS column added
#' 
#' @export

read.aircraft = function(fn){
  d = read.csv(fn)
  d$UNIX_TS = parse_excel_date(d$TheTime)
  d
}