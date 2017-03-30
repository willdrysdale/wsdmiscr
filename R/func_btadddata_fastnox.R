#' BT Tower Add Data 2
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
#' @param o3_new_data raw output from envirolgger when selecting o3 conc only
#' @param nox_new_data crit output file from AQD NOx instrument
#' @param version supply version number of data
#' 
#' @return dataframe with new CSV reformatted and appended to the end, with QC flags applied
#' 
#' @export


btnox_adddata2 = function(main_series,o3_new_data,nox_new_data,version){
  #Format Ozone Data
  o3_new_data = o3_new_data[,1:2]
  names(o3_new_data) = c("date","o3")
  o3_new_data$date = dmy_hm(o3_new_data$date)
  #Format NOx Data
  nox_new_data = parse_fastnox_toconc(nox_new_data)
  #Format Main Series
  main_series$date = ymd_hms(main_series$date)
  #Average to 15 mins and round
  o3_new_data = timeAverage(o3_new_data,avg.time = "15 min")
  nox_new_data = timeAverage(nox_new_data,avg.time = "15 min")
  o3_new_data$date = round_date(o3_new_data$date,unit = "15 min")
  nox_new_data$date = round_date(nox_new_data$date,unit = "15 min")
  #Determine Earliest Start of new data
  if (o3_new_data$date[1] >= nox_new_data$date[1]){
    start = o3_new_data$date[1]
  }else{
    start = nox_new_data$date[1]
  }
  #determine latest end of new data
  if (o3_new_data$date[nrow(o3_new_data)] >= nox_new_data$date[nrow(nox_new_data)]){
    end = o3_new_data$date[nrow(o3_new_data)]
  }else{
    end = nox_new_data$date[nrow(nox_new_data)]
  }
  #Pad new nox
  nox_new_data = padtimeseries(nox_new_data,start,end,"15 min")
  #Merge both datasets into one file
  new_data = merge(nox_new_data,o3_new_data,by = "date",all = T)
  names(new_data) = c("date","no","no2","nox","o3")
  #Flag Data
  new_data = flagdata(new_data,2:5)
  #Merge in new data
  main_series <- bind_rows(main_series, new_data)
  #Pad Main Series
  start = main_series$date[1]
  main_series <- padtimeseries(main_series, start, end, "15 min")
  
  #write New File
  filename = ncas_filename(main_series,"AQD_NOx_Detector","BT-Tower",version)
  savefileprompt(main_series,filename)
  #Return
  return(main_series)
}