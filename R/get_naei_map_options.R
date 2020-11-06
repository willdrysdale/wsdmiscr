#' Get NAEI map options
#' 
#' get the list of species that are avaliable for download - used by \code{get_naei_map()}
#' and useful for the user to match exact names
#' 
#' @param url url of emissions map selection page
#' 
#' @author W. S. Drysdale
#' 
#' @export

get_naei_map_options = function(url ="https://naei.beis.gov.uk/data/map-uk-das"){
  
  require(rvest)
  require(dplyr)
  require(xml2)
  
  page = read_html(url) 
  
  option_nodes = page %>%
    html_nodes("option")
  
  #
  tibble(name = html_text(option_nodes), 
         value = html_attr(option_nodes,"value"))
}