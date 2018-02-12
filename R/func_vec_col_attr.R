#' Vector of Column Attributes
#' 
#' returns a character vector of a specified attribute for each column in a dataframe
#' 
#' @param x dataframe
#' @param which attribute to be returned
#' 
#' @author W S. Drysdale

vec_col_attr = function(x,which){
  attr_na = function(x, which){
    myattr = attr(x,which)
    if(is.null(myattr))
      myattr = NA
    #return
    myattr
  }
  unname(sapply(x, function(x) attr_na(x,"unit"),USE.NAMES = F))
}




