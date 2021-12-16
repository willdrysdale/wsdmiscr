#' tidy rle
#' 
#' Take the output of \code{rle()}, convert to a tibble and calculate start and end indicies.
#' 
#' @param rleObj output of \code{rle()}
#' 
#' @export
#' 
#' @author W. S. Drysdale

tidy_rle = function(rleObj){
  
  require(dplyr)
  
  rleDf = tibble(lengths = rleObj$lengths,
                 values = rleObj$values) %>% 
    mutate(idxStart = cumsum(c(1,lengths))[1:(nrow(.))],
           idxEnd = cumsum(lengths))
  
  #
  rleDf
}