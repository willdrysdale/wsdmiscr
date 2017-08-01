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
  calranges = find_ranges(d,22)
  zeroranges = find_ranges(d,32)
  
  #ask user to state whether the cal should be used or not
  calranges = plyr::adply(calranges, 1, function(x) 
    good_cal_prompt(x), .progress = "none")
  #name the column headers
  names(calranges) = c("start","startrow","end","endrow","good")
  
  #Initialise a new flag column 
  d$cal_flag = 0 
  d$cal_on = 0
  d$zero_flag = 0
  #Flag the calibrations that the user has defined as good with 1
  for (i  in 1:nrow(calranges)){
    range_on = (calranges$startrow[i]-1):(calranges$endrow[i]+1)
    range_flag = calranges$startrow[i]:calranges$endrow[i]
    d$cal_on[range_on] = 1
    if(calranges$good[i] == 1){
      d$cal_flag[range_flag] = 1
    }
  }
  #Flag the Zeros
  range_zero = (zeroranges$startrow[1]):(zeroranges$endrow[1]+5)
  d$zero_flag[range_zero] = 1
  
  for (i in 2:(nrow(zeroranges)-1)){
    range_zero = (zeroranges$startrow[i]-1):(zeroranges$endrow[i]+5)
    d$zero_flag[range_zero] = 1
  }
  range_zero = (zeroranges$startrow[nrow(zeroranges)]-1):(zeroranges$endrow[i])
  d$zero_flag[range_zero] = 1
  
  
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
  
  if (nrow(calranges_good) > 1){
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
    
    range = (previous_cal_row+1):new_cal_row
    
    d$cal_obs[range] = seq(2:(length(range)+1))
    d$cal_obs[range] = d$cal_obs[range]+1
    d$ch1_sens_adj[range] = sens1+(sens1_inc*d$cal_obs[range])
    d$ch2_sens_adj[range] = sens2+(sens2_inc*d$cal_obs[range])
    d$no2_ce_adj[range] = ce+(ce_inc*d$cal_obs[range])
    
    
    #if (nrow(calranges_good) > 2){
    #Middle Intances of Recalculating
    for (i in 2:nrow(calranges_good)){
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
      
      range = (previous_cal_row+1):new_cal_row
      
      d$cal_obs[range] = seq(2:(length(range)+1))
      d$cal_obs[range] = d$cal_obs[range]+1
      d$ch1_sens_adj[range] = sens1+(sens1_inc*d$cal_obs[range])
      d$ch2_sens_adj[range] = sens2+(sens2_inc*d$cal_obs[range])
      d$no2_ce_adj[range] = ce+(ce_inc*d$cal_obs[range])
    }
    #}
    #Final Instance of Recalculating
    previous_cal_row = new_cal_row
    range = previous_cal_row:nrow(d)
    
    d$cal_obs[range] = seq(2:(length(range)+1))
    d$cal_obs[range] = d$cal_obs[range]+1
    d$ch1_sens_adj[range] = d$ch1_sens_adj[previous_cal_row-1]
    d$ch2_sens_adj[range] = d$ch2_sens_adj[previous_cal_row-1]
    d$no2_ce_adj[range] = d$no2_ce_adj[previous_cal_row-1]
  }else{
    #Only one Calibration
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
    
    range = (previous_cal_row+1):new_cal_row
    
    d$cal_obs[range] = seq(2:(length(range)+1))
    d$cal_obs[range] = d$cal_obs[range]+1
    d$ch1_sens_adj[range] = sens1+(sens1_inc*d$cal_obs[range])
    d$ch2_sens_adj[range] = sens2+(sens2_inc*d$cal_obs[range])
    d$no2_ce_adj[range] = ce+(ce_inc*d$cal_obs[range])
    
    #Single Calibration to end
    previous_cal_row = new_cal_row
    d$cal_obs[previous_cal_row] = 1
    range = previous_cal_row:nrow(d)
    
    d$cal_obs[range] = seq(2:(length(range)+1))
    d$cal_obs[range] = d$cal_obs[range]+1
    d$ch1_sens_adj[range] = d$ch1_sens_adj[previous_cal_row-1]
    d$ch2_sens_adj[range] = d$ch2_sens_adj[previous_cal_row-1]
    d$no2_ce_adj[range] = d$no2_ce_adj[previous_cal_row-1]
    
  }
  #Recalculate Concentrations
  
  d$NO_Conc_adj = (d$CH2_Hz - d$zero2)/d$ch2_sens_adj
  d$NO2_Conc_adj = (((d$CH1_Hz - d$zero1)/d$ch1_sens_adj)-d$NO_Conc_adj)/d$no2_ce_adj
  d$NOx_Conc_adj = d$NO_Conc_adj + d$NO2_Conc_adj
  
  #Apply Flags
  d$NO_Conc_adj[d$cal_on == 1] = NA
  
  d$NO2_Conc_adj[d$cal_on == 1] = NA
  
  d$NOx_Conc_adj[d$cal_on == 1] = NA
  
  d$NO_Conc_adj[d$zero_flag == 1] = NA
  
  d$NO2_Conc_adj[d$zero_flag == 1] = NA
  
  d$NOx_Conc_adj[d$zero_flag == 1] = NA
  
  return(d)
}