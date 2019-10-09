#' Read 1D netCDF
#' 
#' reads in a netecesf file as a dataframe, using 1 dimention only
#' 
#' @param path path too ncdf
#' @param var_names if only specific variables are required, supply a vector here
#' @param dim_name name of dimention to read in by
#' 
#' @export


read.1D_ncdf = function(path,var_names,dim_name = "time"){
  #Open Connnection to ncdf file
  nc = nc_open(path)
  #if custom variable names are undefined, read all variables
  if(missing(var_names))
    var_names = names(nc$var)
  #Load Dimention
  
  dim = tryCatch({
    ncvar_get(nc,dim_name)},
    error = function(e){
      "dim_load_error"
    })
  
  if("dim_load_error" %in% dim){
    dim =  nc$var[[1]][["dim"]][[1]]$vals
  }
  
  dim = data.frame(dim)
  names(dim) = dim_name

  var_list = purrr::map(var_names,ncvar_get,nc = nc)
  var_length = purrr::map_int(var_list,length)
  
  vars = var_list[var_length == nrow(dim)] %>% 
    bind_cols()
  
  names(vars) = var_names
  
  dim = cbind(dim,vars)
  
  #close ncdf
  nc_close(nc)
  #return
  dim
}
