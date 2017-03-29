#' Splitting Fucntion
#' 
#' Used to subset data in certain scenarios where subset() and cutdata() fell short
#' 
#' @param df dataframe of time series passed by previous subsetting function 
#' @param variable column by which df should be split
#' 
#' @return list of dataframes

########## Splitting Function ##############
lets_split <- function(df, variable) {
  
  # Split into list
  list_split <- split(df, df[, variable])
  
  # Return
  list_split
}