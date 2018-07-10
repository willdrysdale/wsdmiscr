#' Between operator
#' 
#' Is a value between a pair of values, adapted from https://stackoverflow.com/questions/19441092/how-can-i-create-an-infix-between-operator \cr
#' Exclusive of bounds, irespective of order of rhs. Will use max and min values of rhs
#' 
#' 
#' @usage \code{x %b% c(a,b)}
#' 
#' @param lhs value or vector of values to comapre with rhs
#' @param rhs pair of values in a vector
#' 
#' @export


`%b%`<-function(x,rng){
  x > min(rng) & x < max(rng)
}