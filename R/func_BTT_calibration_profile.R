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
  flist = flist[!str_detect("all data",flist)]
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
  crit = BTT_cal_clean(crit)
  
  #drop extra columns
  crit = crit[,c("UNIX_TS_min","nom_sens_1","nom_ce","nom_sens_2","ch1_sens_adj","ch2_sens_adj","no2_ce_adj")]

  #Define netCDF dimention
  timedim = ncdf4::ncdim_def("date","seconds since 1970-1-1",as.numeric(crit$UNIX_TS_min))
  #Define netCDF variable
  nc_vars = list(
    nom_sens_1_var = ncdf4::ncvar_def("nom_sens_1","",timedim,NA,prec = "double"),
    nom_ce_var = ncdf4::ncvar_def("nom_ce","",timedim,NA,prec = "double"),
    nom_sens_2_var = ncdf4::ncvar_def("nom_sens_2","",timedim,NA,prec = "double"),
    ch1_sens_adj_var = ncdf4::ncvar_def("ch1_sens_adj","",timedim,NA,prec = "double"),
    ch2_sens_adj_var = ncdf4::ncvar_def("ch2_sens_adj","",timedim,NA,prec = "double"),
    no2_ce_adj_var = ncdf4::ncvar_def("no2_ce_adj","",timedim,NA,prec = "double")
  )
  #Create netCDF file
  nc_name = paste0(output_file_path,"\\","calibration_profile",".nc")
  nc_file = ncdf4::nc_create(nc_name,nc_vars,force_v4 = T)
  #Put variables
  ncvar_put(nc_file,nc_vars$nom_sens_1_var,crit$nom_sens_1)
  ncvar_put(nc_file,nc_vars$nom_ce_var,crit$nom_ce)
  ncvar_put(nc_file,nc_vars$nom_sens_2_var,crit$nom_sens_2)
  ncvar_put(nc_file,nc_vars$ch1_sens_adj_var,crit$ch1_sens_adj)
  ncvar_put(nc_file,nc_vars$ch2_sens_adj_var,crit$ch2_sens_adj)
  ncvar_put(nc_file,nc_vars$no2_ce_adj_var,crit$no2_ce_adj)
  
  #Attr
  ncatt_put(nc_file,0,"title","Calibration Profile for UoY AQD NOx Instrument Manny Bianco, during his time at the BT Tower in 2017")
  #close
  nc_close(nc_file)
}
  