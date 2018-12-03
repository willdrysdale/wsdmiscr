#' c_
#' 
#' Updated \code{c()} for binding POSIXct. Preserves timezone (againist normal \code{c()} behaviour). If all tz are the same, use that tz, else use UTC
#' 
#' @param ... objects to be concatenated.
#' 
#' @author W. S. Drysdale
#' 
#' @export

c_ = function(...){
  arguments = list(...)
  tz_check = unique(unlist(lapply(arguments,tz)))
  
  if(length(tz_check) > 1)
    tz = "UTC"
  else
    tz = tz_check
  
  .POSIXct(c(unlist(lapply(list(...), unclass))),tz = tz)
}
