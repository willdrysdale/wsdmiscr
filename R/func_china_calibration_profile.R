#' China Calibraion Profile
#' 
#' From the 1 min AQD crit file, a profile of the corrected calibration paramaters is produced
#' 
#' @param output_file_path Location containing the folder where all of the crit files are stored
#' @param crit_file_folder name of folder located within output_file_path contining crit files
#' @param percentage_drift to be supplied to BTT_cal_clean to limit drift of bad cals
#' 
#' @export
#' 
#' @author Will S. Drysdale

china_calibration_profile = function(output_file_path = "~/../Google Drive/PhD/Freya's Magic Files/CHina/",
                                   crit_file_folder = "minute_data",percentage_drift = 50)
{
  #Read and bind all 60s crit files
  flist = dir(paste(output_file_path,crit_file_folder,sep = "\\"))
  crit = read.csv(paste(output_file_path,crit_file_folder,flist[1],sep = "\\"))
  for (i in 2:length(flist)){
    file_name = paste(output_file_path,crit_file_folder,flist[i],sep = "\\")
    t = read.csv(file_name)
    crit = rbind(crit,t)
  }
  #Create unix Timestamp - nearest 1 min
  crit$UNIX_TS = waclr::parse_excel_date(crit$TheTime)
  crit$UNIX_TS_min = round_date(crit$UNIX_TS,"1 mins")
  crit$UNIX_TS_min = as.numeric(crit$UNIX_TS_min)
  
  #Padtime series on the Unix timestamp only
  ut = seq(crit$UNIX_TS_min[1],crit$UNIX_TS_min[nrow(crit)],60)
  ut = data.frame(ut)
  names(ut) = "UNIX_TS_min"
  crit = left_join(ut,crit,"UNIX_TS_min")
  
  #Generate Linear Drifted sensitivities and CE
  crit$NO2_ConEff[1] = crit$nom_ce[1]
  crit$NO2_ConEff[crit$NO2_ConEff > 1] = NA
  crit$NO2_ConEff[crit$NO2_ConEff < 0.5] = NA
  crit$NO2_ConEff = zoo::na.locf(crit$NO2_ConEff)
  
  crit$CH1_sens[1] = crit$ch1_nom_sens[1]
  crit$CH1_sens[crit$ch1_nom_sens > 1] = NA
  crit$CH1_sens = zoo::na.locf(crit$ch1_nom_sens)
  
  crit$CH2_sens[1] = crit$ch2_nom_sens[1]
  crit$CH2_sens[crit$ch2_nom_sens > 1] = NA
  crit$CH2_sens = zoo::na.locf(crit$ch2_nom_sens)
  
  crit = china_cal_flags(crit)
  crit = BTT_cal_clean(crit,percentage_drift)
  
  crit = crit[,c(1,27,28,29)]
  
  #drop extra columns
  write.csv(crit,paste(output_file_path,"calibration_profile",sep = "\\"),row.names = F)
}
