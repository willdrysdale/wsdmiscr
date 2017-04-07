#' Convert Met Data
#' 
#' Converts wind vectors u and v into windspeed and direction.
#' Also converts Temperature / Speed of sound to  / Celcius
#' 
#' @param d Dataframe containing u = "u", v = "vv", temperature = "sonic_temp"
#' 
#' @return Dataframe with ws,wd and temp columns appended
#' 
#' @export


convert_met_data = function(d){
  plyr::adply(d, 1, function(x)
    convert_met_data_wrk(x), .progress = "text")
}