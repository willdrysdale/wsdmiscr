#' Get NAEI Timeseries
#' 
#' Downloads NAEI emissions timeseries from naei.beis.gov.uk/data/data-selector
#' 
#' @param species vector of pollutant names as listed by \code{get_naei_timeseries_options()}
#' @param from start year
#' @param end year
#' @param file_path path to save downloaded data to. If NULL data will be downloaded to a tempfile and returned
#' 
#' @author W. S. Drysdale
#' 
#' @export



get_naei_timeseries = function(species = "Nitrogen oxides (NOx expressed as NO2)",
                               from = 1970,
                               to = 2019,
                               file_path = NULL,
                               include_activity_data = FALSE
){
  
  require(rvest)
  require(dplyr)
  require(stringr)
  require(purrr)
  require(tidyr)
  
  if(from < 1970){
    from = 1970
  }
  
  if(is.null(file_path)) {
    usingTempFile = TRUE
    file_path = tempfile()
  }else{
    usingTempFile = FALSE
  }
  
  
  urlsTable = build_naei_urls_table()
  
  opts = get_naei_timeseries_options()
  
  selectedOpts = opts %>% 
    filter(name %in% species) %>% 
    nest_by(grp)
  
  if(nrow(selectedOpts) == 0){
    stop("No matching pollutants selected")
  }
  
  requiredUrls = urlsTable %>% 
    filter(grp %in% selectedOpts$grp) %>% 
    left_join(selectedOpts,"grp")
  
  tempFiles = list()
  datList = list()
  for(i in 1:nrow(requiredUrls)){
    session = session(requiredUrls$url[i])
    
    form = html_form(session)[[2]]
    
    if(to > max(as.numeric(form$fields$year_to$options))){
      to = max(as.numeric(form$fields$year_to$options))
    }
    
    filled_form = html_form_set(form, 
                             `pollutant_class_id[]` = requiredUrls$data[[i]]$value,
                             year_from = from, 
                             year_to = to,
                             include_ad = ifelse(include_activity_data,"true","false")
    )
    
    d = session_submit(session, form = filled_form)
    
    a_nodes = d %>% html_nodes("a")
    
    links = tibble(name = html_text(a_nodes), url = html_attr(a_nodes, 
                                                              "href"))
    download = paste0("https://naei.beis.gov.uk/data/",
                      filter(links, str_ends(url, "ukdata"))$url)
    
    tempFiles[[i]] = tempfile()
    
    download.file(download, destfile = tempFiles[[i]], mode = "wb")
    
    if(include_activity_data){
      headerLength = NULL
      thisLineNumber = 1
      
      # scan for end of header
      while(is.null(headerLength)){
        thisLineData = readr::read_lines(tempFiles[[i]], skip = thisLineNumber-1, n_max = 1)
        if(str_detect(thisLineData,"<BR>")){
          headerLength = thisLineNumber-1
        }else{
          thisLineNumber = thisLineNumber+1
        }
        
      }
      
      datList[[i]] = read.csv(tempFiles[[i]], skip = headerLength, na.strings = c("NA","-","")) %>% 
        dplyr::rename(Gas = X.BR.Gas)
      
    }else{
      datList[[i]] = read.csv(tempFiles[[i]], na.strings = c("NA","-","")) 
    }
    
  }
  
  dat = datList %>% 
    bind_rows() %>% 
    tibble() %>% 
    pivot_longer(-c(Gas:Units),
                 names_to = "year") %>% 
    mutate(year = str_remove(year, "X") %>% 
             as.numeric()) 
  
  
  if(usingTempFile){
    return(dat)
  }else{
    write.csv(dat, file_path, row.names = F)
  }
  
}
