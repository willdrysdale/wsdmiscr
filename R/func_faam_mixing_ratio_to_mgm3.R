#' faam_mixing_ratio_to_mgm3
#' 
#' Take a faam merge file and converts a species as a mixing ratio to mg/m3
#' 
#' @param df faam merge file.csv
#' @param unit current unit of mixing ratio. supports "ppm", "ppb", "ppt" (trillion not thousand)
#' @param pollutant column header of pollutant to be converted
#' @param pollutant_mass pollutant mass in mg/mol
#' 
#' @return column of converted data 
#' 
#' @export
#' 
#' @author W S. Drysdale
#' @author Beth Nelson


faam_mixing_ratio_to_mgm3 = function(df,unit,pollutant,pollutant_mass){
  if(unit == "ppm")
    unit_conv = 1000
  if(unit == "ppb")
    unit_conv = 1
  if(unit == "ppt" )
    unit_conv = 1/1000
  
  gas_constant = 8.314
  avagadro_constant = 6.022e23
  
  data = df[,c(pollutant,"ps_rvsm","tat_di_r")]
  
  data[,pollutant] = data[,pollutant]*unit_conv
  
  data[,"ps_rvsm"] = data[,"ps_rvsm"]*100
  
  data$molpervol = data[,"ps_rvsm"]/(gas_constant*data[,"tat_di_r"])
  
  #conversion factor
  data$molpervol = (data$molpervol*avagadro_constant)/1e9
  
  data$mgm3 = (data$molpervol*data[,pollutant])/avagadro_constant
  
  data$mgm3 = data$mgm3*pollutant_mass
  
  data$mgm3
  
}