#' BT Fast NOx add met data
#' 
#' Takes raw AQD NOx output and appends the met data to an ongoing file
#' 
#' @param new_met raw dataframe from AQD NOx
#' @param old_met ongoing met file
#' 
#' @return New Dataframe with new met appended at the end
#' 
#' @export

btnox_adddata2_met = function(new_met,old_met,version){
  #----House keeping----
  #format date
  names(new_met)[1] = "date"
  new_met$date = parse_excel_date(new_met$date)
  old_met$date = ymd_hms(old_met$date)
  #convert wind vectors and temperature as speed of sound to ws,wd and temp in celcius
  new_met = convert_met_data(new_met)
  #Average to 15 min and round
  new_met = timeAverage(new_met, avg.time = "15 min")
  new_met$date = round_date(new_met$date, "15 min")
  #drop columns
  new_met = new_met[,c("date","ws","wd","temp")]
  #-----Merge----
  data = bind_rows(old_met,new_met)
  start = min(data$date)
  end  = max (data$date)
  data = padtimeseries(data, start,end,"15 min")
  filename = ncas_filename(data,"AQD_NOx_Detector","BT-Tower_met",version)
  savefileprompt(data,filename)
  return(data)
}