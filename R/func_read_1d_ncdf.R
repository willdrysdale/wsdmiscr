#' Read 1D netCDF
#' 
#' reads in a netecesf file as a dataframe, using 1 dimention only
#' 
#' @param path path too ncdf
#' @param var_names if only specific variables are required, supply a vector here
#' @param dim_name name of dimention to read in by if dim_name is time, format is assumed to be seconds since 1970-01-01 \n
#' override with \code{origin}
#' @param tz defaults to UTC
#' @param origin defaults to 1970-01-01


read_1D_ncdf = function(path,var_names,dim_name = "time",tz = "UTC",origin = "1970-01-01"){
  #Open Connnection to ncdf file
  nc = nc_open(path)
  #if custom variable names are undefined, read all variables
  if(missing(var_names))
    var_names = names(nc$var)
  #Load Dimention
  dim = ncvar_get(nc,dim_name)
  if(dim_name == "time")
    dim = as.POSIXct(dim,tz = tz,origin = origin)
  dim = data.frame(dim)
  names(dim) = dim_name
  #For each variable
  for (var in var_names){
    temp_var = ncvar_get(nc,var)
    dim = cbind(dim,temp_var)
  }
  #return
  dim
}
