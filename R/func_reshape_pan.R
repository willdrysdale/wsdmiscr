#' Reshape PAN
#' 
#' Takes a dataframe of PAN data, which is in a pseudo wide format and \n
#' reshapes it into long, based of know lags relative to the time stamp \n
#' Input data should be in the form: \n
#' 1. Date (DMY) \n
#' 2. Time (HMS) \n
#' ... \n
#' 7. PAN1A (or PAN1B) \n
#' 8. PAN2A (or PAN2B) \n
#' 9. PAN3A (or PAN3B) \n
#' 10. PAN1A_ppb (or PAN1B_ppb) \n
#' 11. PAN2A_ppb (or PAN2B_ppb) \n
#' 12. PAN3A_ppb (or PAN3B_ppb) \n
#'  Columns 3 - 6 inclusive are not used but expected
#'  
#'  @param d data frame of PAN data
#'  @param ecd either 1 or 2, depending on ECD channel
#'  
#'  @return long PAN data
#'  
#'  @author W S. Drysdale

reshape_pan = function(d,ecd){
  if(missing(ecd))
    stop("Please define ecd as 1 or 2")
  if(ecd != 1 & ecd !=2)
    stop("Please define ecd as 1 or 2")
  if(ecd == 1)
    names(d)[c(1,10:12)] = c("date","pan1a_ppb","pan2a_ppb","pan3a_ppb")
  if(ecd == 2)
    names(d)[c(1,10:12)] = c("date","pan1b_ppb","pan2b_ppb","pan3b_ppb")  
  names(d) %<>% tolower
  d$date = lubridate::dmy(d$date)+lubridate::hms(d$time)
  d = d[!is.na(d$date),]
  
  if(ecd == 1){
    for (i in 1:nrow(d)){
      #ECD1_1A is 1.5 mins behind the previous timestamp
      #If this is the first entry in the file, ECD1_1A cannot be assigned a time
      start_time = d$date[i]
      if (i == 1){
        pan1a = data.frame(date = NA,pan_area = NA,pan_ppb = NA)
        pan2a = data.frame(date = start_time-(7.5*60),pan_area = d$pan2a[i],pan_ppb = d$pan2a_ppb[i])
        pan3a = data.frame(date = start_time-(4.5*60),pan_area = d$pan3a[i],pan_ppb = d$pan3a_ppb[i])
        long = pan1a %>%
          rbind(pan2a) %>%
          rbind(pan3a)
      }else{
        prev_start = d$date[i-1]
        pan1a = data.frame(date = prev_start-(1.5*60),pan_area = d$pan1a[i],pan_ppb = d$pan1a_ppb[i])
        pan2a = data.frame(date = start_time-(7.5*60),pan_area = d$pan2a[i],pan_ppb = d$pan2a_ppb[i])
        pan3a = data.frame(date = start_time-(4.5*60),pan_area = d$pan3a[i],pan_ppb = d$pan3a_ppb[i])
        long %<>% rbind(pan1a) %>%
          rbind(pan2a) %>%
          rbind(pan3a)
      }
    }
  }
  
  if(ecd == 2){
    for (i in 1:nrow(d)){
      start_time = d$date[i]
      if (i == 1){
        pan1b = data.frame(date = start_time-(9*60),pan_area = d$pan1b[i],pan_ppb = d$pan1b_ppb[i])
        pan2b = data.frame(date = start_time-(6*60),pan_area = d$pan2b[i],pan_ppb = d$pan2b_ppb[i])
        pan3b = data.frame(date = start_time-(3*60),pan_area = d$pan3b[i],pan_ppb = d$pan3b_ppb[i])
        long %<>% rbind(pan1b) %>%
          rbind(pan2b) %>%
          rbind(pan3b)
      }else{
        pan1b = data.frame(date = start_time-(9*60),pan_area = d$pan1b[i],pan_ppb = d$pan1b_ppb[i])
        pan2b = data.frame(date = start_time-(6*60),pan_area = d$pan2b[i],pan_ppb = d$pan2b_ppb[i])
        pan3b = data.frame(date = start_time-(3*60),pan_area = d$pan3b[i],pan_ppb = d$pan3b_ppb[i])
        long %<>% rbind(pan1b) %>%
          rbind(pan2b) %>%
          rbind(pan3b)
      }
    }
  }
  long$date = parse_unix_time(long$date)
  #Return
  long
}