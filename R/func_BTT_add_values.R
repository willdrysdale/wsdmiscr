#' BT Tower Update Concentration and Met data 
#' 
#' makes use of btnox_adddata2, btnox_adddata2_met and the btconfig file to append new concntration and met data from the AQD NOX and O3 and the BT Tower
#' 
#' @param o3_new_data Ozone data from envirologger
#' @param new_AQD_data crit files from fast NOx
#' @param conc_version integer for concentraion file version
#' @param met_version integer for met file version
#' 
#' @export

BTT_update_conc_met = function(o3_new_data,new_AQD_data,conc_version,met_version){
  #Locate files
  btfilepath = source(system.file("extdata/btconfig.R",package = "wsdmiscr"))
  btfilepath = btfilepath[[1]]
  #If there are multiple files, select the correct one
  flist = dir(paste(btfilepath,"conc/",sep = ""))
  if (length(flist) > 1){
    print(flist)
    i = as.numeric(choosefileprompt(flist))
  } else 
    i = 1
  #Append new concentration data
  setwd(paste(btfilepath,"conc/",sep = ""))
  main_series = read.csv(flist[i])
  main_series = btnox_adddata2(main_series,o3_new_data,new_AQD_data,conc_version)
  #If there are multiple files, select the correct one
  flist = dir(paste(btfilepath,"met/",sep = ""))
  if (length(flist) > 1){
    print(flist)
    i = as.numeric(choosefileprompt(flist))
  } else 
    i = 1
  #Append new met data
  setwd(paste(btfilepath,"met/",sep = ""))
  old_met = read.csv(paste(flist[i],sep = ""))
  old_met = btnox_adddata2_met(new_AQD_data,old_met,met_version)
  
  return(NULL)
}
  