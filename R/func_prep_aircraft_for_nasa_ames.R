#' Prepare Aircraft Final for NASA Ames
#' 
#' Takes the _final output of \code{wsdmiscr::process.aircraft2()} and preps it for \code{wsdmiscr::create_nasa_ames_1D()}
#' 
#' @param d_final _final output of \code{wsdmiscr::process.aircraft2()}
#' @param missing_flag what should NAs be replaced with
#' @param col_names what should the columns be renamed too
#' @param date_col which column contains the datetime info
#' @param date_parse function to transform the current date into a POSIXct
#' @param origin does date_parse require an origin? include it here
#' 
#' @export

prep_aircraft_final_for_nasa_ames = function(d_final,
                                             missing_flag = 99999,
                                             col_names = NULL,
                                             date_col = 1,
                                             date_parse = waclr::parse_excel_date,
                                             origin = NULL
                                             ){
  if(length(col_names) == 0 )
    col_names = names(d_final)
  if(length(col_names) != ncol(d_final))
    stop("number of col_names does not equal number of new names")
  names(d_final) = col_names
  
  if(length(origin) == 0)
    d_final[,date_col] %<>% date_parse
  if(length(origin) == 1)
    d_final[,date_col] %<>% date_parse(origin = origin)
  if(length(origin) > 1)
    stop("origin must be of length 0 or 1")
  
  d_final[is.na(d_final)] = missing_flag
  day_of_mission = date(d_final[1,date_col])
  
  hours = hour(d_final[,date_col])*3600
  mins = minute(d_final[,date_col])*60
  secs = second(d_final[,date_col])
  
  sec_since_midnight = hours+mins+secs
  
  d_final[,date_col] = round_any(sec_since_midnight,1,floor)
  
  
  out = list(
    df = d_final,
    date = day_of_mission
  )
  #return
  out
}
