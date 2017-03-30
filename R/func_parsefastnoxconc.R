parse_fastnox_toconc = function(d){
  d = d[,c(1,8,5,4)]
  names(d) = c("date","no","no2","nox")
  d = d[!duplicated(d),]
  d$date = parse_excel_date(d$date)
  d$no = d$no/1000
  d$no2 = d$no2/1000
  d$nox = d$nox/1000
  return(d)
}