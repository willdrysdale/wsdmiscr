#' China_raw_data_processing
#' 
#' Takes the raw output of the fast AQD NOx instruments from China, and is supplied with values for 
#' channel 1 + 2 sensitivity and a calibration profile for ce returns a file ready to be run in custom EC code. 
#' Built ontop of the orginal script by Adam R. Vaughan for BTT(2012) and the Beijing Site(2017)
#' 
#' @param d Raw AQD Output
#' @param my_sens1 channel 1 sensitvity
#' @param my_sens2 channel 2 sensitvity
#' @param calibration_profile calibration_profile from china_calibration_profile()
#' 
#' @return data.frame of the format to pass to custom EC code
#' 
#' @export
#' 
#' @author Will S. Drysdale


China_raw_data_processing = function(d,my_sens1,my_sens2,calibration_profile){
  
  #Clean Inf introduced by calibration and zero
  d$NO2_Conc[d$NO2_Conc == Inf] = NA
  
  d$NO_Conc[d$NO_Conc == Inf] = NA
  
  d$CH1_Hz[d$NOx2_conc == Inf] = NA
  d$CH1_Hz[d$NO2_Conc == Inf] = NA
  d$CH1_Hz[d$NO_Conc == Inf] = NA
  
  d$CH1_Hz[d$NOx_Conc == Inf] = NA
  d$CH1_Hz[d$NO2_Conc == Inf] = NA
  d$CH1_Hz[d$NO_Conc == Inf] = NA
  
  d$CH1_Hz[d$NO2_Conc == Inf] = NA
  
  d$CH1_Hz[d$NO_Conc == Inf] = NA
  
  d$CH2_Hz[d$NOx2_conc == Inf] = NA
  d$CH2_Hz[d$NO2_Conc == Inf] = NA
  d$CH2_Hz[d$NO_Conc == Inf] = NA
  
  d$CH2_Hz[d$NOx_Conc == Inf] = NA
  d$CH2_Hz[d$NO2_Conc == Inf] = NA
  d$CH2_Hz[d$NO_Conc == Inf] = NA
  
  d$CH2_Hz[d$NO2_Conc == Inf] = NA
  
  d$CH2_Hz[d$NO_Conc == Inf] = NA
  
  #Create a unix_timestamp
  #Create unix Timestamp - nearest 1 min
  d$UNIX_TS = waclr::parse_excel_date(d$TheTime)
  d$UNIX_TS_min = round_date(d$UNIX_TS,"1 mins")
  d$UNIX_TS_min = as.numeric(d$UNIX_TS_min)
  
  #Find Zero values
  zeros = find_ranges(d,23)
  ch1_zero = mean(d$CH1_Hz[(zeros$startrow+5):zeros$endrow])
  ch2_zero = mean(d$CH2_Hz[(zeros$startrow+5):zeros$endrow])
  
  #Select the Portion of the calibration profile that applies to this file
  begin = d$UNIX_TS_min[1]
  end = d$UNIX_TS_min[nrow(d)]
  cal_pro = calibration_profile[(calibration_profile$UNIX_TS_min >= begin) & (calibration_profile$UNIX_TS_min <= end),]
  
  d <- dplyr::left_join(d, cal_pro, by = "UNIX_TS_min")
  
  #Recalculate concentrations
  d$NO_Conc_adj = (d$CH1_Hz - ch1_zero)/my_sens1
  d$NO2_Conc_adj = (((d$CH2_Hz - ch2_zero)/my_sens2)-d$NO_Conc_adj)/d$no2_ce_adj
  d$NOx_Conc_adj = d$NO_Conc_adj + d$NO2_Conc_adj
  
  #remove any superfluous rows
  d =  d[!is.na(d[, "UNIX_TS"]), ]
  
  #Filter negatives
  d$NOx_Conc_adj[d$NO_Conc_adj < 0] = NA
  d$NOx_Conc_adj[d$NO2_Conc_adj < 0] = NA
  d$NO_Conc_adj[d$NO_Conc_adj < 0] = NA
  d$NO2_Conc_adj[d$NO2_Conc_adj < 0] = NA
  d$NOx_Conc_adj[d$NOx_Conc_adj < 0] = NA
  
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
  
  slow_Temp <- d$temp_sonic
  fst_SONIC_T <- d$temp_sonic
  
  #Pressure
  slow_p <- 1013.25 * ((1 - ((0.0065 * 177) / (d$temp + (0.0065 * 177) + 273.15)) )^5.257) ## tower pressure in hPa
  slow_p <- slow_p * 100
  
  #RH and specific humidity 
  RH <- 50
  press_pa <- slow_p
  
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
  
  fst_FD_mole_NO1_insitu <- d$NO_Conc_adj * 1e-12
  fst_FD_mole_NO2_insitu <- d$NO2_Conc_adj * 1e-12
  
  fst_FD_mole_CO_insitu = co_5hz * 1e-9
  
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