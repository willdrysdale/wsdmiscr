#' Summarise ACF
#' 
#' Summarise the ACF output by a function (e.g median) per scalar
#' 
#' @param acf
#' @param .f
#' @param freq
#' @param ...
#' 
#' @author W. S. Drysdale
#' 
#' @export

summarise.acf = function(acf,.f = median,freq = 5,...){
  acf$date = ymd_hms(acf$date,tz = tz)
  acf$index = acf$date %>% as.factor() %>% as.numeric()
  acf = select(acf,-date) %>% 
    split(acf$index)
  
  acf_lag = function(df){
    vars = names(df)
    vars = vars[!vars %in% c("index","lag")]
    
    lag = c()
    for(i in 1:length(vars)){
      max_acf = suppressWarnings(max(df[,vars[i]],na.rm = T))
      
      if(!is.infinite(max_acf))
        lag = c(lag,df$lag[df[,vars[i]] == max(df[,vars[i]],na.rm = T)])
      else
        lag = c(lag,NA)
    }
    names(lag) = vars
    lag %>% t %>% data.frame()
  }
  
  lag = map_df(acf,acf_lag) %>% 
    summarise_all(.f,...)
  
  lag/freq
  
  
}
