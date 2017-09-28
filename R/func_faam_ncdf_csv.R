#' Faam NetCDF to csv
#' 
#' Takes the NetCDF for FAAM flight data and returns the key inforamtion as a csv
#' 
#' @param ncdf_filepath location of the netCDF
#' 
#' @return returns a .csv with the timestamp in the "date" column in POSIXct format. Also contains "UNIX_TS" unix timestamp and "SECS" seconds since midnight
#' 
#' @export
#' 
#' @author W S Drysdale


read.faam_ncdf = function(ncdf_filepath){
  core_ncdf = nc_open(ncdf_filepath)
  #Strip date and flight id
  filename = core_ncdf$filename
  if(str_detect((filename),"/")){
    filename = str_split(filename,"/")
    filename = filename[[1]][length(filename[[1]])]
  }
  filename = str_split(filename, "_")
  day = as.POSIXct(ymd(filename[[1]][3],tz = "UTC"))
  flight = filename[[1]][6]
  
  #create unix and POSIXct timestamps
  seconds_since_midnight = as.numeric(core_ncdf$dim$Time$vals)
  date = day + seconds_since_midnight
  output_data = data.frame(
    date = date,
    UNIX_TS = as.numeric(date),
    SECS = seconds_since_midnight
  )
  
  #Select variables to merge from core_ncdf
  selected_vars = c("ALT_GIN","BSC_BLUU","BSC_GRNU","BSC_REDU","CO_AERO","CPC_CNTS","HGT_RADR","LAT_GIN","LON_GIN",
                    "O3_TECO","PS_RVSM","TAT_DI_R","TDEW_GE","TSC_BLUU","TSC_GRNU","TSC_REDU","U_C","U_NOTURB",
                    "V_C","V_NOTURB","W_C","WOW_IND")
  
  for (i in 1:length(selected_vars)){
    variable = as.numeric(ncdf4::ncvar_get(core_ncdf,selected_vars[i]))
    output_data = cbind(output_data,variable)
  }
  
  names(output_data)[4:ncol(output_data)] = selected_vars
  names(output_data) = tolower(names(output_data))
  nc_close(core_ncdf)
  output_data
}