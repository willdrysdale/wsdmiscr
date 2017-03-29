#' Aggregate Met Data from date Span
#' 
#' Given averaged data with span ranges in "date_start" and "date_end "columns and met data with "date" column, 
#' average met data over span range and append to data frame
#'  
#' @param df dataframe containing span range
#' @param df_met dataframe containg met data
#' @param progress supply "time" for progress bar
#'  
#' @export

aggregate_by_date_span <- function(df, df_met, progress = "none") {
  
  plyr::adply(df, 1, function(x) 
    aggregate_by_date_span_worker(x, df_met), .progress = progress)
  
}