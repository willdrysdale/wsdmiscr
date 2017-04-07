#' Convert Wind Vectors
#'
#' when provided a dataframe row by row via adply with wind vectors 
#' u and v in columns labled u and vv (DAQfactory format) creates 
#' two new columns ws and wd
#' Note, output is from a gill windmaster pro,where u aligns north positive and v aligns WEST positive 
#' 
#' @param d dataframe row by row via adply
#' 
#' @export

convert_wind_vectors = function(d){
  #parse NA
  if (is.na(d$vv) | is.na(d$u)){
    d$ws = NA
    d$wd = NA
    return(d)
  }
  #Wind Speed
  if (!d$vv == 0 | !d$u == 0)
    d$ws = sqrt((d$vv*d$vv)+(d$u*d$u))
  else
    d$ws = 360
  #Invert v component
  d$vv = -d$vv
  #Zero Cases  
  if(d$u == 0){
    if(d$vv == 0)
      d$wd = 360
    if(d$vv > 0)
      d$wd = 90
    if(d$vv < 0)
      d$wd = 270
    #Restore v
    d$vv = -d$vv
    return(d)
  }
  if(d$vv == 0){
    if(d$u == 0)
      d$wd = 360
    if(d$u > 0)
      d$wd = 0
    if(d$u < 0)
      d$wd = 180
    #Restore v
    d$vv = -d$vv
    return(d)
  }
  #d$wd = atan2(d$u,d$vv)*360/2/pi
  #  if(d$wd < 0)
#    d$wd = d$wd+360
 # return(d)
  
  #None Zero Case
  if(d$u > 0 & d$vv > 0){
    d$wd = atan(d$vv/d$u)
    d$wd = d$wd*(180/pi)
    #Restore v
    d$vv = -d$vv
    return(d)
  }
  if(d$u > 0 & d$vv < 0){
    d$wd = atan(d$u/d$vv)
    d$wd = d$wd*(180/pi)
    d$wd = 270+(d$wd*-1)
    #Restore v
    d$vv = -d$vv
    return(d)
    
  } 
  if(d$u < 0 & d$vv > 0){
    d$wd = atan(d$u/d$vv)
    d$wd = d$wd*(180/pi)
    d$wd = 90+(d$wd*-1)
    #Restore v
    d$vv = -d$vv
    return(d)
  }
  if(d$u < 0 & d$vv < 0){
    d$wd = atan(d$u/d$vv)
    d$wd = d$wd*(180/pi)
    d$wd = 270-d$wd
    #Restore v
    d$vv = -d$vv
    return(d)
  }
}
    
    