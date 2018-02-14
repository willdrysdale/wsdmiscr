#' BT Tower Calibration Clean
#' 
#' As the BTT calibrations performed automatically, and are less likley to be incorrect than an Aircraft Calibration, the user
#' is not asked whether they are good or bad on a per cal basis. This function provides some broad error catching for bad cals.
#' If the calibration parameters drift outside of a set percetange away from the average *nominal*, they are converted to NA
#' 
#' @param d dataframe of bound crit files
#' @param percent_drift numerical value of percentage drift allowed i.e 50 percent is provided as 50
#' 

BTT_cal_clean = function(d,upper_stdev_scalar = 1.5,lower_stdev_scalar = 0.5){
  #Calculate bounds from the mean
  ch1_sens_upper = median(d$ch1_sens_adj,na.rm = T) + (sd(d$ch1_sens_adj,na.rm = T)*upper_stdev_scalar)
  ch1_sens_lower = median(d$ch1_sens_adj,na.rm = T) - (sd(d$ch1_sens_adj,na.rm = T)*lower_stdev_scalar)
  
  ch2_sens_upper = median(d$ch2_sens_adj,na.rm = T) + (sd(d$ch2_sens_adj,na.rm = T)*upper_stdev_scalar)
  ch2_sens_lower = median(d$ch2_sens_adj,na.rm = T) - (sd(d$ch2_sens_adj,na.rm = T)*lower_stdev_scalar)
  
  ce_upper = median(d$no2_ce_adj,na.rm = T) + (sd(d$no2_ce_adj,na.rm = T)*upper_stdev_scalar)
  ce_lower = median(d$no2_ce_adj,na.rm = T) - (sd(d$no2_ce_adj,na.rm = T)*lower_stdev_scalar)
  
  #if drifts exceed these bounds turn them to NA
  d$ch1_sens_adj[(d$ch1_sens_adj > ch1_sens_upper) | (d$ch1_sens_adj < ch1_sens_lower)] = NA
  d$ch2_sens_adj[(d$ch2_sens_adj > ch2_sens_upper) | (d$ch2_sens_adj < ch2_sens_lower)] = NA
  d$no2_ce_adj[(d$no2_ce_adj > ce_upper) | (d$no2_ce_adj < ce_lower)] = NA
  
  #If the first of any calibration factor is removed by the filters, replace it with the median until there is a calibration
  if(is.na(d$ch1_sens_adj[1]))
    d$ch1_sens_adj[1] = median(d$ch1_sens_adj,na.rm = T)
  if(is.na(d$ch2_sens_adj[1]))
    d$ch2_sens_adj[1] = median(d$ch2_sens_adj,na.rm = T)
  if(is.na(d$no2_ce_adj[1]))
    d$no2_ce_adj[1] = median(d$no2_ce_adj,na.rm = T)
  
  #Stamp over NA'd calibrations with previous calibration
  d$ch1_sens_adj = na.locf(d$ch1_sens_adj)
  d$ch2_sens_adj = na.locf(d$ch2_sens_adj)
  d$no2_ce_adj = na.locf(d$no2_ce_adj)
  
  #return
  d
}