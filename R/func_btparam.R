#' BT Tower Parameter Monitor
#' 
#' Creates a PDF of key paramaters on a time plot. Saves in current working directory
#' 
#' @param d Dataframe containing the raw NOx_crit output file - or a group of the bound together
#' 
#' @export


bt_fastnox_param_monitor = function(d){
  names(d)[1] = "date"
  d$date = parse_excel_date(d$date)
  
  start_date = as.character(d$date[1])
  start_date = gsub(" ","_",start_date)
  start_date = gsub(":","",start_date)
  start_date = gsub("-","",start_date)
  
  end_date = as.character(d$date[nrow(d)])
  end_date = gsub(" ","_",end_date)
  end_date = gsub(":","",end_date)
  end_date = gsub("-","",end_date)
  
  filename = paste(start_date,"__",end_date,"_btnoxparams.pdf",sep = "")
  pdf(file = filename,width = 10,height = 15)
  
  timePlot(d,pol = c("CH1_zero","CH2_zero","nom_sens_1","CH1_sens","nom_sens_2","CH2_sens","nom_ce"
                     ,"NO2_ConEff","PMT_Temp","Control_Temp","Rxn_Vessel_Pressure","blc_temp"),y.relation = "free")

  dev.off()

}