#' Unique Day Comparions Full
#' 
#' Performs full day comparison experiment using exp_unique_event and exp_unique_event_day
#' and create a PDF of results
#' 
#' @param d Dataframe containing time series
#' @param dtraj back trajectories covering time period to be studied
#' @param date DD/MM/YYYY of unique day
#' @param wbefore Number of weeks before unique date
#' @param wafter number of weeks after unique date
#' @param dbefore number of days before unique date (If not supplied, equal to wbefore)
#' @param dafter number of days before unique date (If not supplied, equal to wafter)
#' @param pol single pollutant to perfom experiment on
#' @param period over which the time series has been averaged (used to pad data series)
#' @param use_polcol T/F whether to use colours returned by pollution colour for consistency, currently supports no, no2, nox
#' 
#' @export

exp_day_comparison = function(d,dtraj,date = "24/04/2016",wbefore = 4,wafter = 4,dbefore,dafter, pol = "nox",period = "15 min", use_polcol = T){
  if (missing(dbefore))
    dbefore = wbefore
  if(missing(dafter))
    dafter = wafter
  
  
  
  date2 = dmy(date)
  
  weekscomparison = exp_unique_event(d,date2,wbefore,wafter,pol,period)
  dayscomparison = exp_unique_event_days(d,date2,dbefore,dafter,pol,period)
  daysbcomparison = exp_unique_event_days(d,date2,dbefore,0,pol,period)
  daysacomparison = exp_unique_event_days(d,date2,0,dafter,pol,period)

  polx = paste(pol,".x",sep = "")
  poly = paste(pol,".y",sep = "")
  npolavgweeks = paste("Average ",pol," over previous ",wbefore," and following ",wafter," weeks",sep = "")
  npolavgdays = paste("Average ",pol," over previous ",dbefore," and following ",dafter," days",sep = "")
  npolavgdaysb = paste("Average ",pol," over previous ",dbefore," days",sep = "")
  npolavgdaysa = paste("Average ",pol," over following ",dafter," days",sep = "")
  
  npoluniqueday = paste(pol," on ",date,sep = "")
  
  polcol = list(
    blue = "no.x",
    darkblue = "no.y",
    purple = "no2.x",
    deeppurple = "no2.y",
    red = "nox.x",
    darkred = "nox.y"
  )
  
  laby = ""
  labx = c("","","","")
  
  if(use_polcol == T){
  pdf(NULL)
  
    tv = list(
      timeVariation(weekscomparison,
                    pol = c(polx,poly),
                    name.pol = c(npolavgweeks,npoluniqueday),
                    cols = pollution_colour(c(polx,poly),polcol),
                    key = T,
                    ylab = laby,
                    xlab = labx
                    ),
      timeVariation(dayscomparison,
                    pol = c(polx,poly),
                    name.pol = c(npolavgdays,npoluniqueday),
                    cols = pollution_colour(c(polx,poly),polcol),
                    key = T,
                    ylab = laby,
                    xlab = labx
                    ),
      timeVariation(daysbcomparison,
                    pol = c(polx,poly),
                    name.pol = c(npolavgdaysb,npoluniqueday),
                    cols = pollution_colour(c(polx,poly),polcol),
                    key = T,
                    ylab = laby,
                    xlab = labx
                    ),
      timeVariation(daysacomparison,
                    pol = c(polx,poly),
                    name.pol = c(npolavgdaysa,npoluniqueday),
                    cols = pollution_colour(c(polx,poly),polcol),
                    key = T,
                    ylab = laby,
                    xlab = labx
                    )
    )
    
    names(tv) = c("weeks","days","daysb","daysa")
    
    dev.off()
  }else{
    pdf(NULL)
    
    tv = list(
      timeVariation(weekscomparison,
                    pol = c(polx,poly),
                    name.pol = c(npolavgweeks,npoluniqueday),
                    key = T,
                    ylab = laby,
                    xlab = labx
      ),
      timeVariation(dayscomparison,
                    pol = c(polx,poly),
                    name.pol = c(npolavgdays,npoluniqueday),
                    key = T,
                    ylab = laby,
                    xlab = labx
      ),
      timeVariation(daysbcomparison,
                    pol = c(polx,poly),
                    name.pol = c(npolavgdaysb,npoluniqueday),
                    key = T,
                    ylab = laby,
                    xlab = labx
      ),
      timeVariation(daysacomparison,
                    pol = c(polx,poly),
                    name.pol = c(npolavgdaysa,npoluniqueday),
                    key = T,
                    ylab = laby,
                    xlab = labx
      )
    )
    
    names(tv) = c("weeks","days","daysb","daysa")
    
    dev.off()
  }
  
  filename = paste("Uniqueday",pol,gsub("/","",date),".pdf",sep = "_")
  pdf(filename,width = 9)
  
  plot(tv$weeks$plot$hour)
  plot(tv$days$plot$hour)
  plot(tv$daysb$plot$hour)
  plot(tv$daysa$plot$hour)
  plot(tv$daysb$plot$day.hour)
  plot(tv$daysa$plot$day.hour)
  if(!missing(dtraj))
    trajPlot(selectByDate(dtraj, start = date,end = date))
  
  dev.off()
}
