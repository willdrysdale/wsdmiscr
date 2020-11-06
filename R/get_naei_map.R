#' Get NAEI map
#' 
#' Download the NAEI UK emissions map
#' 
#' @param species name of species of interest as written the in dropdown box at \url{https://naei.beis.gov.uk/data/map-uk-das}
#' @param write_dir file path to save data to. Map is downloaded to \code{tempdir()} if NULL.
#' 
#' @author W. S. Drysdale
#' 
#' @export


get_NAEI_map = function(species = "Nitrogen oxides (NOx expressed as NO2)",
                        write_dir = NULL){
  
  require(rvest)
  require(dplyr)
  require(stringr)
  require(xml2)
  
  url <- "https://naei.beis.gov.uk/data/map-uk-das" # go here
  
  session = html_session(url) # connect
  
  form = html_form(session)[[2]] # get form
  
  options = get_naei_map_options()
  
  filled_form <- set_values(form, # fill form
                            "pollutant_id" = filter(options,
                                                    name == species)$value
  )
  
  d <- submit_form(session=session,form=filled_form, POST=url) # submit form
  
  
  # get download url
  a_nodes = d %>%
    html_nodes("a") 
  
  links = tibble(name = html_text(a_nodes),
                 url = html_attr(a_nodes,"href"))
  
  
  download = filter(links,
                    str_detect(name,"Area"))$url # could make into option type, if you want the effort of differnt readers
  
  if(is.null(write_dir)){
    write_dir = tempdir()
  }
  
  download.file(download,destfile = file.path(write_dir,tail(str_split(download,"/")[[1]],1)),mode = "wb") # save file to temp 
  
  # unzip
  unzip(zipfile = file.path(write_dir,tail(str_split(download,"/")[[1]],1)),
        exdir = str_remove_all(file.path(write_dir,tail(str_split(download,"/")[[1]],1)),".zip"))
  
  # remvoe zip
  file.remove(file.path(write_dir,tail(str_split(download,"/")[[1]],1)))
  
  # read and return
  load_naei_layers(str_replace(file.path(write_dir,tail(str_split(download,"/")[[1]],1)),".zip","/"))
  
}
