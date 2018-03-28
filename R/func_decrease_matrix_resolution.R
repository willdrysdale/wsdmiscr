#' decrease_matrix_resolution
#' 
#' @export


decrease_matrix_resolution = function(mymatrix,factor = 10){
  old_dim = dim(mymatrix)
  new_dim = old_dim/factor  
  if(F %in% (new_dim%%1 == c(0,0)))
    stop("New dimentions are none-integer, change factor or resize input matrix")
  new_matrix = matrix(nrow = new_dim[1],ncol = new_dim[2])
  i_val = seq(1,old_dim[1],factor)
  j_val = seq(1,old_dim[2],factor)
  for(i in 1:new_dim[1]){
    for(j in 1:new_dim[2]){
      new_matrix[i,j] = mean(mymatrix[i_val[i]:(i_val[i]+(factor-1)),j_val[j]:(j_val[j]+(factor-1))],na.rm = T)
      }
  }
  #return
  new_matrix
}