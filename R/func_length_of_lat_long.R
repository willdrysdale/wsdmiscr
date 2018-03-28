#' Length of Latitude/Longditue
#' 
#' Based on the latitude, calculate the size of degree lat or long in meters \cr
#' using equations from https://en.wikipedia.org/wiki/Latitude#Length_of_a_degree_of_latitude \cr
#' and https://en.wikipedia.org/wiki/Longitude#Length_of_a_degree_of_longitude
#' 
#' @param deg_lat degrees latitude for distance calculation
#' @param r radius of earth, default 6378137 (\code{length_of_long()} only)
#' 
#' @return Size of 1 degree in m
#' 
#' @author W. S. Drysdale
#' 
#' @export length_of_long length_of_lat


length_of_long = function(deg_lat,r = 6378137){
  deg2rad <- function(deg) {(deg * pi) / (180)}
  pi/180 * r * cos(deg2rad(deg_lat))
}

length_of_lat = function(deg_lat){
  deg2rad <- function(deg) {(deg * pi) / (180)}
  rad_lat = deg2rad(deg_lat)
  111132.954 - (559.822*cos(2*rad_lat)) + (1.175 * cos(4* rad_lat))
}