#' Lazy Read NASA Ames
#' 
#' Function to read NASA Ames files into R in a basic way
#' 
#' @param file path of NASA Ames File
#' @param start_date date where DOY begins
#' @param column where date is stored, default 1
#' @param tz timezone for date_from_decimal_day
#' 
#' @return list where item one is the header text and item 2 is the data as a dataframe
#' 
#' @export
#' 
#' @author Will S. Drysdale

lazy_read_nasa_ames = function(file,start_date,date_column = 1,tz = "UTC"){
  warning("lazy_read_nasa_ames() is depreciated - please use read_nasa_ames()")
  header_length = readLines(file,1)
  header_length = stringr::str_split(header_length," ")
  header_length = as.numeric((header_length[[1]][1]))-1
  file_header = readLines(file,header_length)
  file_data = read.table(file,skip = header_length,header = T)
  file_data$date = date_from_decimal_day(file_data[,date_column],start_date,tz = tz)
  file_list = list(header = file_header,file = file_data)
  #return
  file_list
}
