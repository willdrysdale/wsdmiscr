#' BTT_EP_raw_data_processing
#' 
#' Takes the raw output of the fast AQD NOx instruments at the BTT, and when supplied a calibration profile, output
#' by BTT_calibration_profile and the processed met data from the Reading data downloader - returns a file ready to be run
#' in Eddy Pro. Built ontop of the orginal script by Adam R. Vaughan for BTT(2012) and the Beijing Site(2017)
#' 
#' @param d Raw AQD Output
#' @param calibration_profile profile produced by BTT_calibration_profile from 1 min crit files
#' @param extra_met output of BTT_parse_1hz_met
#' 
#' @return data.frame of the format to pass to Eddy Pro
#' 
#' @export
#' 
#' @author Will S. Drysdale


BTT_EP_raw_data_processing = function(d,calibration_profile,extra_met){
  
  #When Zero Valves are NA make them 0
  d$zero_valve_1[is.na(d$zero_valve_1)] = 0
  d$zero_valve_2[is.na(d$zero_valve_2)] = 0
  
  #Clean Inf introduced by calibration and zeros
  d$NOx2_conc[d$NOx2_conc == Inf] = NA
  d$NOx2_conc[d$NO2_Conc == Inf] = NA
  d$NOx2_conc[d$NO_Conc == Inf] = NA
  
  d$NOx_Conc[d$NOx_Conc == Inf] = NA
  d$NOx_Conc[d$NO2_Conc == Inf] = NA
  d$NOx_Conc[d$NO_Conc == Inf] = NA
  
  d$NO2_Conc[d$NO2_Conc == Inf] = NA
  
  d$NO_Conc[d$NO_Conc == Inf] = NA
  
  #Create a unix_timestamp
  d$UNIX_TS = as.numeric(waclr::parse_excel_date(d$TheTime))
  #Create a unix_timestamp rounded to the nearest minute
  d$UNIX_TS_min = waclr::parse_unix_time(d$UNIX_TS)
  
  d$UNIX_TS_min = floor_date(d$UNIX_TS_min, "min")
  
  d$UNIX_TS_min = as.numeric(d$UNIX_TS_min)
  #Select the Portion of the calibration profile that applies to this file
  begin = d$UNIX_TS_min[1]
  end = d$UNIX_TS_min[nrow(d)]
  cal_pro = calibration_profile[(calibration_profile$UNIX_TS_min >= begin) & (calibration_profile$UNIX_TS_min <= end),]
  
  cal_pro = cal_pro[,c(1,2,6,7,8)]
  names(cal_pro)[2] = "TheTime_cp"
  
  d <- dplyr::left_join(d, cal_pro, by = "UNIX_TS_min")
  
  #Recalculate concentrations
  d$NO_Conc_adj = (d$CH2_Hz - d$CH2_zero)/d$ch2_sens_adj
  d$NO2_Conc_adj = (((d$CH1_Hz - d$CH1_zero)/d$ch1_sens_adj)-d$NO_Conc_adj)/d$no2_ce_adj
  d$NOx_Conc_adj = d$NO_Conc_adj + d$NO2_Conc_adj
  
  #Filter negatives
  d$NOx_Conc_adj[d$NO_Conc_adj < 0] = NA
  d$NOx_Conc_adj[d$NO2_Conc_adj < 0] = NA
  d$NO_Conc_adj[d$NO_Conc_adj < 0] = NA
  d$NO2_Conc_adj[d$NO2_Conc_adj < 0] = NA
  d$NOx_Conc_adj[d$NOx_Conc_adj < 0] = NA
  
  #remove any superfluous rows
  d =  d[!is.na(d[, "UNIX_TS"]), ]
  
  #Select the Portion of the met data that applies to this file
  met_pro = extra_met[(extra_met$UNIX_TS_min >= begin) & (extra_met$UNIX_TS_min <= end),]
  met_pro = na.omit(met_pro)
  met_pro = met_pro[,c(2:8)]
  met_pro$UNIX_TS_min = round(met_pro$UNIX_TS_min)
  d <- dplyr::left_join(d, met_pro, by = "UNIX_TS_min")
  
  #Create POSIXct Time
  the_time = waclr::parse_excel_date(d$TheTime)
  df_the_time = data.frame(date = the_time)
  #calculates day of year + decimal day
  DOY = wsdmiscr::get_DOY(df_the_time)
  
  #calculates hour of day + decimal hour
  UTC <- as.POSIXlt(the_time)
  UTC <- UTC$hour + UTC$min/60 + UTC$sec/3600
  
  #set fast U wind measurement
  fst_u <- d$u
  
  #set fast V wind measurement
  fst_v <- d$vv
  
  #set fast W wind measurement
  fst_w <- d$w
  
  #set temperature measurements, slow temp from slow met and fast temp converted from speed of sound
  slow_Temp <- d$slow_temp
  
  #d = plyr::adply(d, 1, function(x) convert_temp_sos_c(x))
  #fst_SONIC_T <- d$temp
  fst_SONIC_T <- d$sonic_temp
  
  #set pressure at the tower
  slow_p <- d$pressure
  
  #set RH at the tower
  RH <- d$rh
  
  # calculate water vapour mixing ratio
  t <- fst_SONIC_T
  press <- slow_p
  
  #water vapor pressure E/e[hPa] from temperature temp[degC]:
  #Constants for Magnus formula
  cM1<-6.11
  cM2<-17.08
  cM3<-234.18
  #Saturation water vapor pressure:
  E<-cM1 * exp((cM2*t) / (cM3+t))
  
  #water vapor pressure e[hpa] from saturation water vapor pressure E[hPa] and relative humidity RH[%]:
  e<-E*RH/100
  
  #specific humidity q[kg kg-1] from water vapor pressure e[hPa] and static pressure p[hPa]:
  #calculation
  q<-0.62198*e/(press-0.37802*e)
  humidity <- q
  
  fst_FD_mole_H2O_insitu <- humidity
  fst_FD_mole_H2O_hut <- humidity
  
  fst_FD_mole_NO1_insitu <- d$NO_Conc_adj #* 1e-12
  fst_FD_mole_NO2_insitu <- d$NO2_Conc_adj #* 1e-12
  
  ns.data <- data.frame(
    DOY,
    UTC,
    fst_u,
    fst_v,
    fst_w,
    slow_Temp,
    fst_SONIC_T,
    slow_p,
    fst_FD_mole_H2O_insitu,
    fst_FD_mole_NO1_insitu,
    fst_FD_mole_NO2_insitu,
    fst_FD_mole_H2O_hut,
    uls_z = rep(177,nrow(d)),
    ABL = rep(1500,nrow(d))
  )
  
  row.has.na <- apply(ns.data, 1, function(x){any(is.na(x))})
  ns.data <- ns.data[!row.has.na,]
  
  ns.data$X = 1:nrow(ns.data)
  
  # Ensure table is arranged by date
  #ns.data <- dplyr::arrange(ns.data, DOY)
  
  #return
  ns.data
}