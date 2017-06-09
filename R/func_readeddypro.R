read.eddypro = function(filepath){
  headers = read.csv(filepath,skip = 1,nrow  = 1)
  headers = names(headers)
  file = read.csv(filepath,skip = 3,header = F)
  names(file) = headers
  return(file)
}