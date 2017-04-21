#' Aircraft Cal Flags
#' 
#' When given a dataframe of AQD NOx aircraft data (designed using MOYA Data) locates calibrations,
#' asks the user whether they are good or bad cals, then, using this information, recalculates the concentrations using a linear drift
#' 
#' @param d Raw AQD NOx output
#' 
#' @export


aircraft_cal_flags = function(d){
  #Create Table of ranges where calibration valve is active
  calranges = find_cal_ranges(d)
  
  #ask user to state whether the cal should be used or not
  calranges = plyr::adply(calranges, 1, function(x) 
    good_cal_prompt(x), .progress = "none")
  #name the column headers
  names(calranges) = c("start","startrow","end","endrow","good")
  
  #Initialise a new flag column 
  d$cal_flag = 0 
  #Flag the calibrations that the user has defined as good with 1
  for (i  in 1:nrow(calranges)){
    if(calranges$good[i] == 1){
      for (j in calranges$startrow[i]:calranges$endrow[i])
        d$cal_flag[j] = 1
    }
  }
  
  #Attain the initial nominals
  sens1 = mean(d$nom_sens_1,na.rm = T)
  sens2 = mean(d$nom_sens_2,na.rm = T)
  ce = mean(d$nom_ce,na.rm = T)
  
  #Drop the bad cal ranges
  calranges_good = calranges[!calranges$good==0,]
  
  #Initialise New Columns
  
  d$cal_obs = 1
  d$ch1_sens_adj = sens1
  d$ch2_sens_adj = sens2
  d$no2_ce_adj = ce
  
  #First instance of recalculating 
  previous_cal_row = 1
  new_cal_row = calranges_good$endrow[1]+1
  inter_cal_range = new_cal_row - previous_cal_row
  sens1_inc = (d$ch1_sens[new_cal_row]-sens1)/inter_cal_range
  sens2_inc = (d$ch2_sens[new_cal_row]-sens2)/inter_cal_range
  ce_inc = (d$no2_ce[new_cal_row]-ce)/inter_cal_range
  
  d$cal_obs[previous_cal_row] = 1
  d$ch1_sens_adj[previous_cal_row] = sens1
  d$ch2_sens_adj[previous_cal_row] = sens2
  d$no2_ce_adj[previous_cal_row] = ce
  
  for (i in (previous_cal_row+1):new_cal_row){
    d$cal_obs[i] = d$cal_obs[i-1]+1
    d$ch1_sens_adj[i] = d$ch1_sens_adj[i-1]+sens1_inc
    d$ch2_sens_adj[i] = d$ch2_sens_adj[i-1]+sens2_inc
    d$no2_ce_adj[i] = d$no2_ce_adj[i-1]+ce_inc
  }

  
  #Middle Intances of Recalculating
  for (i in 2:nrow(calranges_good)-1){
    previous_cal_row = new_cal_row
    new_cal_row = calranges_good$endrow[i]+1
    
    sens1 = d$ch1_sens[previous_cal_row]
    sens2 = d$ch2_sens[previous_cal_row]
    ce = d$no2_ce[previous_cal_row]
    
    inter_cal_range = new_cal_row - previous_cal_row
    sens1_inc = (d$ch1_sens[new_cal_row]-sens1)/inter_cal_range
    sens2_inc = (d$ch2_sens[new_cal_row]-sens2)/inter_cal_range
    ce_inc = (d$no2_ce[new_cal_row]-ce)/inter_cal_range
    
    d$cal_obs[previous_cal_row] = 1
    d$ch1_sens_adj[previous_cal_row] = sens1
    d$ch2_sens_adj[previous_cal_row] = sens2
    d$no2_ce_adj[previous_cal_row] = ce
    
    for (j in (previous_cal_row+1):new_cal_row){
      #d$cal_obs[j] = d$cal_obs[j-1]+1
      d$ch1_sens_adj[j] = d$ch1_sens_adj[j-1]+sens1_inc
      d$ch2_sens_adj[j] = d$ch2_sens_adj[j-1]+sens2_inc
      d$no2_ce_adj[j] = d$no2_ce_adj[j-1]+ce_inc
    }
  }
  
  #Final Instance of Recalculating
  previous_cal_row = new_cal_row
  for (j in (previous_cal_row+1):nrow(d)){
    d$cal_obs[j] = d$cal_obs[j-1]+1
    d$ch1_sens_adj[j] = d$ch1_sens_adj[j-1]
    d$ch2_sens_adj[j] = d$ch2_sens_adj[j-1]
    d$no2_ce_adj[j] = d$no2_ce_adj[j-1]
  }
  
  #Recalculate Concentrations
  
  d$NO_Conc_adj = (d$CH2_Hz - d$zero2)/d$ch2_sens_adj
  d$NO2_Conc_adj = (((d$CH1_Hz - d$zero1)/d$ch1_sens_adj)-d$NO_Conc_adj)/d$no2_ce_adj
  d$NOx_Conc_adj = d$NO_Conc_adj + d$NO2_Conc_adj
  
  #Apply Flags
  d$NO_Conc_adj[d$no_conc == -99999.0000] = NA
  d$NO_Conc_adj[d$no_conc == -49999.5000] = NA
  
  d$NO2_Conc_adj[d$no2_conc == -99999.0000] = NA
  d$NO2_Conc_adj[d$no2_conc == -49999.5000] = NA
  
  d$NOx_Conc_adj[d$nox_conc == -99999.0000] = NA
  d$NOx_Conc_adj[d$nox_conc == -49999.5000] = NA
  
  return(d)
}