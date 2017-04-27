#' Flux Pre Processing
#'
#' Given Raw DAQfactory output from the AQD instruments at the BT Tower or China sites, reprocesses the concentration using calibration data
#' for 5Hz and 1 min crit data. 5Hz is reformatted for input into the ERF code
#' 
#' @param d 5hz output file
#' @param site Either "bt" or "china" 

flux_pre_process = function(d,site = "bt"){
  #Confirm Site has been suplpied and is supported
  sites = c("bt","china")
  if (missing(site)){
    print("Please specify site")
    return(NULL)
  }
  if (!site %in% sites){
    print(paste("Site must be one of: ",sites,sep = ""))
    return(NULL)
  }
  
  #BT
  #if (site == "bt")
    #d = #Call BT Processing
  #China
  #if (site == "china")
    #d = #Call China Processing 
  
}