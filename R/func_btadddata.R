#' BT Tower Add Data
#' 
#' Adds data in the raw envirologger output from WACLs NOx box at the BT Tower to main CSV files (main_series) in a specific format
#' columns for main_series: date: converted to POSIXct using ymd_hms()
#'                          no: NO / ppb
#'                          no2: NO2 / ppb
#'                          nox: Total NOx / ppb
#'                          o3: O3 / ppb
#'                          qc_flag_no: QC Flag 0-4
#'                          qc_flag_no2: QC Flag 0-4
#'                          qc_flag_nox: QC Flag 0-4
#'                          qc_flag_o3: QC Flag 0-4
#' Instrument name, location and version number are currently controlled by editing this function, where this infomation is passed to ncas_filename()
#' but could be adapted to have these as arguments in the main function.
#'                          
#' @param main_series dataframe formatted as above
#' @param new_data raw output from envirolgger with "DateTime","NO / ppb","NO2 / ppb","NOx / ppb"."O3 / ppb","" as column headers
#' 
#' @usage btnox_adddata(x,y)
#' 
#' @return dataframe with new CSV reformatted and appended to the end, with QC flags applied
#' 
#' @export


btnox_adddata = function(main_series,new_data){
  new_data = new_data[,1:5]
  names(new_data) = c("date","no","no2","nox","o3")
  #Swap NO2 and NOx columns to reflect new header
  temp = new_data$no2
  new_data$no2 = new_data$nox
  new_data$nox = temp
  #Format Date
  new_data$date = dmy_hm(new_data$date)
  main_series$date = ymd_hms(main_series$date)
  #Average to 15 mins
  new_data = timeAverage(new_data,avg.time = "15 min")
  #Flag Data
  new_data = flagdata(new_data,2:5)
  #Append new_data to main_series
  main_series = rbind(main_series,new_data)
  #write New File
  filename = ncas_filename(main_series,"Teledyne-T200","BT-Tower",1)
  savefileprompt(main_series,filename)
  #Return
  main_series
}