#' BTT Cal Flags
#' 
#' When given a dataframe of AQD BTT NOx data locates calibrations,
#' using this information, recalculates the sensitivites and CE using a linear drift
#' 
#' @param d Raw AQD NOx output
#' 

china_cal_flags = function(d){
  #Create Table of ranges where calibration valve is active
  v = rle(d$NO2_ConEff)
  lengths = cumsum(v$lengths)
  calranges = data.frame(endrow = lengths)
  
  #Initialise a new flag column 
  d$cal_flag = 0 
  
  #Attain the initial nominals
  sens1 = d$CH1_sens[1]
  sens2 = d$CH2_sens[1]
  ce = d$NO2_ConEff[1]
  
  #Initialise New Columns
  
  d$cal_obs = 1
  d$ch1_sens_adj = sens1
  d$ch2_sens_adj = sens2
  d$no2_ce_adj = ce
  
  if (nrow(calranges) > 1){
    #First instance of recalculating 
    previous_cal_row = 1
    new_cal_row = calranges$endrow[1]+1
    inter_cal_range = new_cal_row - previous_cal_row
    sens1_inc = (d$CH1_sens[new_cal_row]-sens1)/inter_cal_range
    sens2_inc = (d$CH2_sens[new_cal_row]-sens2)/inter_cal_range
    ce_inc = (d$NO2_ConEff[new_cal_row]-ce)/inter_cal_range
    
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
    
    
    #Middle Intances of Recalculating
    for (i in 2:nrow(calranges)-1){
      previous_cal_row = new_cal_row
      new_cal_row = calranges$endrow[i]+1
      
      sens1 = d$CH1_sens[previous_cal_row]
      sens2 = d$CH2_sens[previous_cal_row]
      ce = d$NO2_ConEff[previous_cal_row]
      
      inter_cal_range = new_cal_row - previous_cal_row
      sens1_inc = (d$CH1_sens[new_cal_row]-sens1)/inter_cal_range
      sens2_inc = (d$CH2_sens[new_cal_row]-sens2)/inter_cal_range
      ce_inc = (d$NO2_ConEff[new_cal_row]-ce)/inter_cal_range
      
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
    new_cal_row = calranges$endrow[1]+1
    inter_cal_range = new_cal_row - previous_cal_row
    sens1_inc = (d$CH1_sens[new_cal_row]-sens1)/inter_cal_range
    sens2_inc = (d$CH2_sens[new_cal_row]-sens2)/inter_cal_range
    ce_inc = (d$NO2_ConEff[new_cal_row]-ce)/inter_cal_range
    
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
  
  return(d)
}