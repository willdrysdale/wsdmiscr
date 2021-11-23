#' Get NAEI Timeseries Options
#' 
#' Returns the air pollutants available in the timerseries datasets at naei.beis.gov.uk/data/data-selector
#' 
#' @author W. S. Drysdale
#' 
#' @export

get_naei_timeseries_options = function(){
  require(rvest)
  require(dplyr)
  require(purrr)
  
  urlsTable = build_naei_urls_table()
  
  opts = map2_df(urlsTable$url, urlsTable$grp,
                 ~{
                   session = session(.x)
                   
                   form = html_form(session)[[2]]
                   
                   opts = as_tibble(form$fields$`pollutant_class_id[]`$options,rownames = "name") %>% 
                     mutate(grp = .y)
                   
                   #
                   opts
                 }
                 
  )
  #
  opts
}



