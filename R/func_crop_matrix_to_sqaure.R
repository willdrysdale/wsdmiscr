#' Crop Matrix to Square
#' 
#' Crop a matrix to size. Discards a border around the trim_size, preserving the center of the matrix \cr
#' If \code{mymatrix} is smaller than the \code{trim_size}, \code{mymatrix} will be padded with \code{pad} \cr
#' \cr
#' Only supports matrices that both dimentions are to be cropped or padded. will not crop one dimention and pad the other
#' 
#' @param mymatrix matrix to be cropped
#' @param trim_size dimentions to crop matrix
#' @param pad if matrix is to be padded
#' 
#' @export
#' @author W S Drysdale

crop_matix_to_square = function(mymatrix,trim_size = 100,pad = 0){
  dimen = dim(mymatrix)
  border = ceiling(((dimen-trim_size)/2)) #differece between the trim size and current size of matrix
  #postitive means matrix is larger than trim size
  if(border[1] == 0) # if they are equal, make -1,-1 (this may include the -0.5,-0.5 case)
    border = c(-1,-1)
  if(border[1] >= 0) #if the matrix is too large, simply crop the dimentions, keeping the center point the same
    mymatrix = mymatrix[border[1]:(dimen[1]-border[1]),border[2]:(dimen[2]-border[2])]
  else{#if matrix is too small, create a matrix of zeros equal to the trim size, and reverse the above, filling in the 
    #padded footprint's center with the too small footprint
    border = floor(((dimen-trim_size)/2))*-1
    padded_matrix = matrix(0,trim_size,trim_size)
    #Handle an off by one issue introduced when 0.5 indexes occur when dim(matrix)/2 is performed
    if(dim(padded_matrix[border[1]:(dimen[1]+border[1]),border[2]:(dimen[2]+border[2])])[1] > dim(mymatrix)[1])
      padded_matrix[border[1]:(dimen[1]+border[1]-1),border[2]:(dimen[2]+border[2]-1)] = mymatrix
    else
      padded_matrix[border[1]:(dimen[1]+border[1]),border[2]:(dimen[2]+border[2])] = mymatrix
    mymatrix = padded_matrix # assign the padded_matrixprint as the footprint stored
    
    #return
    mymatrix
  } 
}