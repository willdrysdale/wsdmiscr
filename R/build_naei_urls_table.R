#' Build NAEI URLs Table
#' 
#' returns the URLs used for scraping timeseries data
#' 
#' @export

build_naei_urls_table = function(){
  
  require(dplyr)
  
  tribble(
    ~grp,~url,
    "ghg","https://naei.beis.gov.uk/data/data-selector?view=greenhouse-gases",
    "ap","https://naei.beis.gov.uk/data/data-selector?view=air-pollutants",
    "hm","https://naei.beis.gov.uk/data/data-selector?view=heavy-metals",
    "pms","https://naei.beis.gov.uk/data/data-selector?view=pms")
}