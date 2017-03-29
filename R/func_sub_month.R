#' Subset Month
#' 
#' Subsets the data into months, and then applies lower level subsets additionally
#' 
#' @param d dataframe of time series passed by previous subsetting function
#' 
#' @return Large list containing data frames of subsetted months and lists with lower level subsetting applied

########## Month ###########################
sub_month = function(d){
  #Returns Either a dataframe for each month, or a data frame for each month with low level
  #subsetting applied
  #Subset/Split
  n = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec",
        "jan_sub","feb_sub","mar_sub","apr_sub","may_sub","jun_sub","jul_sub",
        "aug_sub","sep_sub","oct_sub","nov_sub","dec_sub")
  m1 = subset(d, d$month == 1)
  m2 = subset(d, d$month == 2)
  m3 = subset(d, d$month == 3)
  m4 = subset(d, d$month == 4)
  m5 = subset(d, d$month == 5)
  m6 = subset(d, d$month == 6)
  m7 = subset(d, d$month == 7)
  m8 = subset(d, d$month == 8)
  m9 = subset(d, d$month == 9)
  m10 = subset(d, d$month == 10)
  m11 = subset(d, d$month == 11)
  m12 = subset(d, d$month == 12)
  
  
  #List Formation
  month = list(
    m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,
    sub_lowlevel(m1),sub_lowlevel(m2),sub_lowlevel(m3),
    sub_lowlevel(m4),sub_lowlevel(m5),sub_lowlevel(m6),
    sub_lowlevel(m7),sub_lowlevel(m8),sub_lowlevel(m9),
    sub_lowlevel(m10),sub_lowlevel(m11),sub_lowlevel(m12)
  )
  names(month) = n
  
  #Return
  month
}