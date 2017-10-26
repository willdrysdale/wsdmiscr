#' mass_balance_pre_origin
#' 
#' Process a faam merge file ready for gridding in origin for mass balance. must have date column
#' 
#' @param flight_data faam merge file with date column
#' @param flight_summary flight sumamry read in by read.faam_fligh_sum
#' @param downwind_runs vector of numbers corresponding to the list entries produced by flight_range_subset() for downwind
#' @param upwind_runs vector of numbers corresponding to the list entries produced by flight_range_subset() for upwind
#' @param missing_flag flag for missing data default -99999
#' @param pollutant string or vector of strings containg pollutants. see faam_mixing_ratio_to_mgm3
#' @param unit string or vector of strings containg units. see faam_mixing_ratio_to_mgm3
#' @param pollutant_mass string or vector of strings containg pollutant mass. see faam_mixing_ratio_to_mgm3
#' 
#' @author Will S. Drysdale
#' @author Beth Nelson
#' 
#' @return list containg the upwind and downwind data.frames
#' 
#' @export


mass_balance_pre_origin = function(flight_data,
                                   flight_summary,
                                   downwind_runs,
                                   upwind_runs,
                                   missing_flag = -99999,
                                   pollutant = "ch4_ppb",
                                   unit = "ppb",
                                   pollutant_mass = 16042){
  
  flight_data = faam_lon_lat.to.east_north(flight_data,missing_flag)
  
  val = c(length(unit),length(pollutant),length(pollutant_mass))
  if(!all.equal(val,rep(val[1],length(val))))
    stop("unit, pollutant and pollutant mass vectors must be of same length")
  for (i in 1:length(unit)){
    if(i == 1)
      cols = data.frame(faam_mixing_ratio_to_mgm3(flight_data,unit[i],pollutant[i],pollutant_mass[i]))
    else{
      temp_col = faam_mixing_ratio_to_mgm3(flight_data,unit[i],pollutant[i],pollutant_mass[i])
      cols = cbind(cols,temp_col)
    }
  }
  names(cols) = paste(pollutant,"_mgm3",sep = "")
  flight_data = cbind(flight_data,cols)

  flight_data_split = flight_range_subset(flight_data,flight_sum)
  
  for (i in 1:length(downwind_runs)){
    if(i == 1){
      flight_data_downwind = flight_data_split[[downwind_runs[i]]]
      flight_data_downwind$event = names(flight_data_split)[downwind_runs[i]]
    }
    
    else
    {
      temp = flight_data_split[[downwind_runs[i]]]
      temp$event = names(flight_data_split)[downwind_runs[i]]
      flight_data_downwind = rbind(flight_data_downwind,temp)
      
    }
    
  }
  
  for (i in 1:length(upwind_runs)){
    if(i == 1){
      flight_data_upwind = flight_data_split[[upwind_runs[i]]]
      flight_data_upwind$event = names(flight_data_split)[upwind_runs[i]]
    }
    
    else
    {
      temp = flight_data_split[[upwind_runs[i]]]
      temp$event = names(flight_data_split)[upwind_runs[i]]
      flight_data_upwind = rbind(flight_data_upwind,temp)
    }
    
  }
  upwind_downwind = list(downwind = flight_data_downwind, upwind = flight_data_upwind)
  
  #return
  upwind_downwind
}
