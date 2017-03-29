#' Aggregate Met Data from date Span
#' 
#' Given averaged data with span ranges in "date_start" and "date_end "columns and met data with "date" column, 
#' average met data over span range and append to data frame
#' passed from wrapper function line by line via adply()
#'  
#' @param df dataframe containing span range
#' @param df_met dataframe containg met data

aggregate_by_date_span_worker <- function(df, df_met) {
  
  # Get dates
  date_start <- df$date_start[1]
  date_end <- df$date_end[1]
  
  # Filter met by dates
  df_met_filter <- df_met[df_met$date >= date_start & df_met$date <= date_end, ]
  
  # Aggregate
  if (nrow(df_met_filter) >= 1) {
    
    # Do it
    df_met_filter_agg <- openair::timeAverage(df_met_filter, avg.time = "year")
    
  } else {
    
    warning(paste("Date start and end do not make sense, skipping row: ",row.names(df)," or no met data exists",sep = ""))
    return(data.frame())
    
  }
  
  # drop unneeded date 
  df_met_filter_agg$date <- NULL
  
  # Join
  df <- bind_cols(df, df_met_filter_agg)
  
  return(df)
  
}






