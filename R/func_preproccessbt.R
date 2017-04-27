

flux_pre_process_bt = function(d){
  #Apply Sensitivities 
  d$NO_Conc = (d$CH2_Hz - d$CH2_zero)/d$CH2_sens
  d$NO2_Conc = (((d$CH1_Hz - d$CH1_zero)/d$CH1_sens)-d$NO_Conc)/d$nom_ce
  d$NOx_Conc = d$NO_Conc + d$NO2_Conc
  #Parse Date
  d$TheTime = parse_excel_date(d$TheTime)
  #Form Day of year
  d$DOY = as.numeric(strftime(d$TheTime, format = "%j"))
}