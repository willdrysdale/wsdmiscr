#' Aircraft Clean Negative
#' 
#' Removes Negatives from the adjusted concentraions from NOx (AQD) aircraft data
#' 
#' @param d Output of aircraft_cal_flags
#' 
#' @export


aircraft_clean_negatives = function(d){
  d$NOx_Conc_adj[d$NO_Conc_adj < 0] = NA
  d$NOx_Conc_adj[d$NO2_Conc_adj < 0] = NA
  d$NO_Conc_adj[d$NO_Conc_adj < 0] = NA
  d$NO2_Conc_adj[d$NO2_Conc_adj < 0] = NA
  d$NOx_Conc_adj[d$NOx_Conc_adj < 0] = NA
  
  #return
  d
}