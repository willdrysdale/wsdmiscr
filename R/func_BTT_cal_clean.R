#' BT Tower Calibration Clean
#' 
#' As the BTT calibrations performed automatically, and are less likley to be incorrect than an Aircraft Calibration, the user
#' is not asked whether they are good or bad on a per cal basis. This function provides some broad error catching for bad cals.
#' If the calibration parameters drift outside of a set percetange away from the average *nominal*, they are converted to NA
#' 
#' @param d dataframe of bound crit files
#' @param percent_drift numerical value of percentage drift allowed i.e 50% is provided as 50

BTT_cal_clean = function(d,percent_drift){
  #Calculate multipliers
  upper = 1+(percent_drift/100)
  lower = 1-(percent_drift/100)
  
  #Determine mean of nominals as starting point
  nom_sens_1 = mean(d$nom_sens_1,na.rm = T)
  nom_sens_2 = mean(d$nom_sens_2,na.rm = T)
  nom_ce = mean(d$nom_ce,na.rm = T)
  
  #Calculate bounds from the mean
  ch1_sens_upper = nom_sens_1*upper
  ch1_sens_lower = nom_sens_1*lower
  
  ch2_sens_upper = nom_sens_2*upper
  ch2_sens_lower = nom_sens_2*lower
  
  ce_upper = nom_ce*upper
  ce_lower = nom_ce*lower
  
  #if drifts exceed these bounds turn them to NA
  d$ch1_sens_adj[(d$ch1_sens_adj > ch1_sens_upper) | (d$ch1_sens_adj < ch1_sens_lower)] = NA
  d$ch2_sens_adj[(d$ch2_sens_adj > ch2_sens_upper) | (d$ch2_sens_adj < ch2_sens_lower)] = NA
  d$no2_ce_adj[(d$no2_ce_adj > ce_upper) | (d$no2_ce_adj < ce_lower)] = NA
  
  #Stamp over NA'd calibrations with previous calibration
  d$ch1_sens_adj = na.locf(d$ch1_sens_adj)
  d$ch2_sens_adj = na.locf(d$ch2_sens_adj)
  d$no2_ce_adj = na.locf(d$no2_ce_adj)
  
  #return
  d
}