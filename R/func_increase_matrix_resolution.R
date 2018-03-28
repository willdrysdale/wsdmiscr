#' increase_matrix_resolution
#' 
#' repeats matrix elements to artificially increase resolution
#' 
#' @param mymatrix input matrix
#' @param size how many times to repeat values row and column wise, 4 will return 16 copies of the value
#' 
#' @export

increase_matrix_resolution = function(mymatrix,size){
  # mymatrix[rep((rows to repeat), c(repeat each row x times)),rep((cols to repeat), c(repeat each col x times))]
  r_mymatrix = mymatrix[rep(1:nrow(mymatrix),rep(size,nrow(mymatrix))) , rep(1:ncol(mymatrix),rep(size,ncol(mymatrix)))]
  #return
  r_mymatrix
}
