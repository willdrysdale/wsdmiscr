#' FAAM core nox merge
#' 
#' Merge nox into the core faam ncdf
#' 
#' @param ncdf_filepath location of the netcdf
#' @param processed_nox nox csv, date can still be in excel time under "TheTime"
#' 
#' @author W S Drysdale
#' 
#' @export

FAAM_core_nox_merge = function(ncdf_filepath,processed_nox){
  #Load the FAAM NetCDF
  output_data = faam_ndcf.to.csv(ncdf_filepath)
  if("TheTime" %in% names(processed_nox) & !"date" %in% names(processed_nox)){
    processed_nox$date = parse_excel_date(processed_nox$TheTime)
    processed_nox = processed_nox[c(2,3,4,5)]
    names(processed_nox) = c("no_conc","no2_conc","nox_conc","date")
  }

  #round dates to avoid strange floating point error then merge
  processed_nox$date = floor_date(processed_nox$date,unit = "seconds")
  output_data$date = floor_date(output_data$date,unit = "seconds")
  output_data = left_join(output_data,processed_nox,"date")
  
  #return
  output_data
}




