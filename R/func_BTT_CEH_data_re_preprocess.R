#' CEH_data_reprocess
#'
#' Alternative preprocess script to supply the 2012-13 data to new flux code.
#' Built ontop of the orginal script by Adam R. Vaughan for BTT(2012) and the Beijing Site(2017)
#'
#'
#' @param d dataframe of raw data
#' @param header dataframe containing the headers for d as its own column headers
#' 
#' @author Will S. Drysdale
#' 
#' @export

BTT_CEH_data_reprocess = function(d,header,filename){
  names(d) = names(header)
  #Prodcue Timestamp
  filename = strsplit(filename,split = "_")
  day = ymd(filename[[1]][3])
  time = filename[[1]][4]
  time = strsplit(time,split = ".asc")
  time = time[[1]][1]
  time = substring(time,c(1,3),c(2,4))
  time = paste(time[1],":",time[2],sep = "")
  date = paste(day,time)
  date = ymd_hm(date)
  
  end_date = date + 3600
  
  d$X = seq(1,nrow(d),1)
  rows = nrow(d)+1
  d$date = date + ((end_date-date)/rows*d$X)
  
  #DOY
  DOY = get_DOY(data.frame(date = d$date))
  
  #UTC
  UTC <- as.POSIXlt(d$date)
  UTC <- UTC$hour + UTC$min/60 + UTC$sec/3600
  
  #Wind
  fst_u = d$u
  fst_v = d$v..m.s.
  fst_w = d$w..m.s.
  
  #Temperature
  slow_Temp = d$Speed.of.sound #use temp corrected file (degrees C)
  fst_SONIC_T <- d$Speed.of.sound
  
  #Pressure
  slow_p <- 1013.25 * ((1 - ((0.0065 * 177) / (d$Speed.of.sound + (0.0065 * 177) + 273.15)) )^5.257) ## tower pressure in hPa
  slow_p <- slow_p * 100
  
  #RH and specific humidity 
  RH <- 50
  press_pa <- slow_p
  
  #water vapor pressure E/e[hPa] from temperature temp[degC]:
  #Constants for Magnus formula
  cM1<-6.11
  cM2<-17.08
  cM3<-234.18
  #Saturation water vapor pressure:
  E<-cM1 * exp((cM2*fst_SONIC_T) / (cM3+fst_SONIC_T))
  
  #water vapor pressure e[hpa] from saturation water vapor pressure E[hPa] and relative humidity RH[%]:
  e<-E*RH/100
  
  #specific humidity q[kg kg-1] from water vapor pressure e[hPa] and static pressure p[hPa]:
  #calculation
  fst_FD_mole_H2O_insitu <-0.62198*e/(press_pa-0.37802*e)
  
  fst_FD_mole_H2O_hut <- fst_FD_mole_H2O_insitu
  
  fst_FD_mole_NO1_ppb <- (d$NO..ppb.)# * 1e-9
  fst_FD_mole_NO2_ppb <- (d$NO2..ppb.)# * 1e-9
  
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
    fst_FD_mole_NO1_ppb,
    fst_FD_mole_NO2_ppb,
    fst_FD_mole_H2O_hut,
    uls_z = rep(177,nrow(d)),
    ABL = rep(1500,nrow(d))
  )
  
  ns.data$fst_FD_mole_NO1_ppb[ns.data$fst_FD_mole_NO1_ppb < 0] = NA
  ns.data$fst_FD_mole_NO2_ppb[ns.data$fst_FD_mole_NO2_ppb < 0] = NA
  ns.data$X = 1:nrow(ns.data)
  
  
  
  return(ns.data)
  }