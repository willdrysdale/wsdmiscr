read_easykrig = function(filename){
  mat = R.matlab::readMat(filename)
  
  mat_dat = list()
  mat_dat$dat = mat$data[[2]][[2]][[5]]
  mat_dat$var_abs = mat$data[[2]][[2]][[6]]*mat$data[[2]][[2]][[5]]
  mat_dat$var_per = mat$data[[2]][[2]][[6]]
  mat_dat$grid_size = mat$para[[9]][[15]][[1]]*mat$para[[9]][[18]][[1]]
  
  
  mat_dat
}