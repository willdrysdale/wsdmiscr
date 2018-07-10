#' Read 1D netCDF
#' 
#' reads in a netecesf file as a dataframe, using 1 dimention only
#' 
#' @param path path too ncdf
#' @param var_names if only specific variables are required, supply a vector here
#' @param dim_name name of dimention to read in by if dim_name is time, format is assumed to be seconds since 1970-01-01 \cr
#' override with \code{origin}
#' @param tz defaults to UTC
#' @param origin defaults to 1970-01-01
#' 
#' @export


read_1D_ncdf = function(path,var_names,dim_name = "time",tz = "UTC",origin = "1970-01-01"){
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
  
  if(dim == "dim_load_error"){
    dim =  nc$var[[1]][["dim"]][[1]]$vals
  }
  
  if(dim_name == "time")
    dim = as.POSIXct(dim,tz = tz,origin = origin)
  dim = data.frame(dim)
  names(dim) = dim_name
  #For each variable
  var_names2 = c()
  for (var in var_names){
    temp_var = ncvar_get(nc,var)
    if(nrow(temp_var) == nrow(dim)){
      dim = cbind(dim,temp_var)
      var_names2 = c(var_names2,var)
    }
  }
  names(dim)[2:ncol(dim)] = var_names2

  
  
  #close ncdf
  nc_close(nc)
  #return
  dim
}
