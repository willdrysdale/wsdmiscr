#' Lubridate Supplements
#' 
#' Lubridate supllements lifted from github.com/wacl-york/waclr

parse_excel_date <- function(x, tz = "UTC", type = "windows") {
  
  # Check
  type <- stringr::str_to_lower(type)
  type <- stringr::str_replace_all(type, "\\.| ", "_")
  
  if (!type %in% c("windows", "os_x_2007")) {
    stop("Type must be 'windows' or 'os_x_2007'", call. = FALSE)
  }
  
  # To numeric
  if (!class(x) == "numeric") x <- as.numeric(x)
  
  # To unix time, different origins depending on version
  if (type == "windows") x <- (x - 25569) * 86400
  if (type == "os_x_2007") x <- (x - 24107) * 86400
  
  # To POSIXct
  x <- parse_unix_time(x, tz = tz)
  
  return(x)
  
}

parse_unix_time <- function(x, tz = "UTC", origin = "1970-01-01") {
  
  # Parse
  x <- as.POSIXct(x, tz = tz, origin = origin)
  
  return(x)
  
}