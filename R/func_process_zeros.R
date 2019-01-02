#' process_zeros
#' 
#' Calculates the result of function f, default median, over a series of state changes of equal length, determined by rle
#' 
#' @param df dataframe containing both the flag and calc data
#' @param zero_flag name(s) of columns to use as the statechange flag
#' @param calc_col  name(s) of columns to use as the data to calculate f from
#' @param state_length length of the state, so they can be selected from the rle result
#' @param trim_beg trim a number of records from the begining of the state change
#' @param trim_end trim a number of records from the end of the state change
#' @param f function uses to calculate the statistic. Default median
#' @param ... additional arguments to pass to f
#' 
#' @export

process_zeros = function(df,
                         zero_flag = c("zero_valve_1","zero_valve_2"),
                         calc_col = c("CH1_Hz","CH2_Hz"),
                         out_col = c("CH1_Hz_zero","CH2_Hz_zero"),
                         state_length = 15,
                         trim_beg = 5,
                         trim_end = 5,
                         f = median,
                         interp = T,
                         ...){
  
  #function for calculating the stat over a range of indexes 
  calc_between_index = function(start,end,df,col = "CH1_Hz",f = median,...){
    df[start:end,col] %>% f(...) 
  }
  
  
  #if the df object is a tibble coerce to df otherwise rle() breaks
  if("tbl_df" %in% class(df))
    df = as.data.frame(df)

  for(i in 1:length(zero_flag)){
    #calculate ranges
    flags = rle(df[,zero_flag[i]])
    flags$cumsum = cumsum(flags$lengths)
    flags$cumsum = flags$cumsum - state_length
    start = flags$cumsum[flags$lengths == state_length]
    start = start+trim_beg
    end = start+(state_length-trim_beg-trim_end)
    zero_range = data.frame(start,end)
    
    #calculate stat
    vals = vector(mode = "numeric",length = nrow(zero_range))
    for(j in 1:nrow(zero_range)){
      vals[j] = calc_between_index(zero_range$start[j],zero_range$end[j],df,calc_col[i],f = f)
    }
    df[,paste0(out_col[i],"_calc")] = NA
    df[zero_range$start,paste0(out_col[i],"_calc")] = vals
    if(interp){
      interp_vals = df[,paste0(out_col[i],"_calc")] %>% na.approx()
      diff = nrow(df)-length(interp_vals)
      first_non_na = which(!is.na(df[,paste0(out_col[i],"_calc")])) %>% min %>% -1
      diff = diff-first_non_na
      interp_vals = c(rep(NA,first_non_na),interp_vals,rep(NA,diff))
      df[,paste0(out_col[i],"_calc")] = interp_vals
    }

  }
  #return
  df
}
