#' BT Tower Add Data - Ozone Only
#' 
#' Adds data in the raw  output from WACLs Ozone box and Fast NOx at the BT Tower to main CSV files (main_series) in a specific format
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
#' @param o3_new_data raw output from envirolgger with "DateTime","NO / ppb","NO2 / ppb","NOx / ppb"."O3 / ppb","" as column headers
#' 
#' @usage btnox_adddata(x,y)
#' 
#' @return dataframe with new CSV reformatted and appended to the end, with QC flags applied
#' 
#' @export


btnox_adddata2 = function(main_series,o3_new_data,nox_new_data,version){
  #Format Ozone Data
  names(o3_new_data) = c("date","o3")
  #Format NOx Data
  nox_new_data = nox_new_data[,c(1,4,5,8)]
  temp = nox_new_data[,4]
  nox_new_data[,4] = nox_new_data[,8]
  nox_new_data[,8] = temp
  names(nox_new_data) = c("date","no","no2","nox")
  #Format Date
  o3_new_data$date = dmy_hm(o3_new_data$date)
  nox_new_data$date = waclr::parse_excel_date(nox_new_data$date)
  main_series$date = ymd_hms(main_series$date)
  #Average to 15 mins and floor
  o3_new_data = timeAverage(o3_new_data,avg.time = "15 min")
  nox_new_data = timeAverage(o3_new_data,avg.time = "15 min")
  o3_new_data$date = floor_date(o3_new_data$date,unit = "15 min")
  nox_new_data$date = floor_date(nox_new_data$date,unit = "15 min")
  #Merge into one file
  #Check that nox data being added is within the same range as the ozone data - for coninuities sake
  if (nox_new_data$date[1] %in% o3_new_data$date)
    new_data = merge(nox_new_data,o3_new_data,by = "date",all = T)
  else{
    print("NOx data being added is outside of O3 date range")
    return(NULL)
  }
  #Flag Data
  new_data = flagdata(new_data,2:5)
  #Append new_data to main_series
  main_series = merge(main_series,new_data,by = "date",all = T)
  #write New File
  filename = ncas_filename(main_series,"AQD_NOx_Detector","BT-Tower",version)
  savefileprompt(main_series,filename)
  #Return
  return(main_series)
}