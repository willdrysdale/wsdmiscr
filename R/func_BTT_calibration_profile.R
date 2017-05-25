#' BTT Calibraion Profile
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

BTT_calibration_profile = function(output_file_path = "C:\\Users\\Will\\Google Drive\\PhD\\site_BT Tower\\AQD_Calibration_Profile",
                                   crit_file_folder = "1_min_crit_files",percentage_drift = 50)
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
  crit$UNIX_TS_min = waclr::parse_excel_date(crit$TheTime)
  crit$UNIX_TS_min = round_date(crit$UNIX_TS,"1 mins")
  crit$UNIX_TS_min = as.numeric(crit$UNIX_TS)
  
  #Padtime series on the Unix timestamp only
  ut = seq(crit$UNIX_TS_min[1],crit$UNIX_TS_min[nrow(crit)],60)
  ut = data.frame(ut)
  names(ut) = "UNIX_TS_min"
  crit = merge(ut,crit,by = "UNIX_TS_min",all = T)
  
  #Generate Linear Drifted sensitivities and CE
  crit = BTT_cal_flags(crit)
  crit = BTT_cal_clean(crit,percentage_drift)
  
  #drop extra columns
  crit = crit[,c(1,2,10,11,12,38,39,40)]
  write.csv(crit,paste(output_file_path,"calibration_profile",sep = "\\"),row.names = F)
}
  