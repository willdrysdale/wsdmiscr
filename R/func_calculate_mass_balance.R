#' calculate_mass_balance
#' 
#' Calculates a mass_balance value at the end of the mass balances script
#' 
#' @param upwind_leg trimmed upwind leg from mass balance pre origin
#' @param downwind_matrix dowwind pollutant origin matrix read in as a dataframe from .csv
#' @param v_matirx v_c origin matrix read in as a dataframe from .csv
#' @param xrows number of x grid lines calculated in mass balances script
#' @param yrows number of y grid lines calculated in mass balances script
#' @param pollutant name of pollutant (single only) in upwind_leg
#' @param grid_scale scale of map grid axis (northing or easting) set in mass balances script
#' @param alt_scale scale of altitude axis set in mass balances script
#' 
#' @return value of mass balance calculation in mg s-1
#' 
#' @author Will S. Drysdale
#' @author Beth Nelson
#' @export


calculate_mass_balance = function(upwind_leg,
                                  downwind_matrix,
                                  v_matrix,
                                  xrows,
                                  yrows,
                                  pollutant,
                                  grid_scale,
                                  alt_scale){
  #Create Upwind matrix (data.frame)
  pol_mean = mean(upwind_leg[,pollutant],na.rm = T)
  pol_mean = rep(pol_mean,yrows)
  pol_mean = data.frame(pol_mean)
  col = pol_mean
  for (i in 1:(xrows-1))
    pol_mean = cbind(pol_mean,col)
    
  upwind = pol_mean

  enhancement = downwind_matrix - upwind
    
  final = enhancement*v_matrix
  
  grid_square_area = grid_scale*alt_scale
  mass_balance = sum((final*grid_square_area))
  
  #return
  mass_balance
}
