#' BTT Cal Flags
#' 
#' When given a dataframe of AQD BTT NOx data locates calibrations,
#' using this information, recalculates the sensitivites and CE using a linear drift
#' 
#' @param d Raw AQD NOx output
#' 

BTT_cal_flags = function(d){
  #clean con eff
  d$NO2_ConEff[d$NO2_ConEff < 0 | d$NO2_ConEff > 1] = NA
  #Create Table of ranges where calibration valve is active
  calranges = find_ranges(d,18)
  
  #Initialise a new flag column 
  d$cal_flag = 0 

  #Attain the initial nominals
  sens1 = mean(d$nom_sens_1,na.rm = T)
  sens2 = mean(d$nom_sens_2,na.rm = T)
  ce = mean(d$nom_ce,na.rm = T)
  
  #popluate where the calibration factors change
  d$ch1_sens_adj = NA
  d$ch2_sens_adj = NA
  d$no2_ce_adj = NA
  for(i in 1:nrow(calranges)){
    row = calranges$endrow[i]+1
    d$ch1_sens_adj[row] = d$CH1_sens[row]
    d$ch2_sens_adj[row] = d$CH2_sens[row]
    d$no2_ce_adj[row] = d$NO2_ConEff[row]
  }
  
  d$ch1_sens_adj[1] = sens1
  d$ch2_sens_adj[1] = sens2
  d$no2_ce_adj[1] = ce
  
  
  lastcal = calranges$endrow[nrow(calranges)]+1
  d$ch1_sens_adj[lastcal:nrow(d)] = d$CH1_sens[lastcal]
  d$ch2_sens_adj[lastcal:nrow(d)] = d$CH2_sens[lastcal]
  d$no2_ce_adj[lastcal:nrow(d)] = d$nom_ce[lastcal]
  
  #linearly interpolate between calibrations
  interp_col = c("ch1_sens_adj","ch2_sens_adj","no2_ce_adj")
  for(var in interp_col)
    d[,var] = na.approx(d[,var],maxgap = nrow(d))
  
  return(d)
}