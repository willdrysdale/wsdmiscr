#' Nest Tabset
#' 
#' Create nested tabsets in RMarkdown. Supply a nested list of objects,\cr
#' unique ids for these and a function to display them. Returns the html to \cr
#' be contained in a r chunk with results = "asis"
#' 
#' @param listTree A list or list of lists which defines the structure of the tabset. \cr 
#'        Names of list levels will be used to name tabs. Double underscore ,"__", will be replaced with a space
#' @param idTree A list or list of list, of character strings. Used for div ids. Must be unique throught the whole markdown document.
#' @param depth What depth should the tabset start at. I.e how many "#" would the top level begin with
#' @param parse_function A function to handle whatever lies at the end of the lists. e.g. use print for ggplots.
#' @param tabset_type either "tabset" or "tabset tabset-pills" Note the lack of curly braces and full stops.
#' 
#' @author W. S. Drysdale
#' 
#' @export

nest_tabset = function(listTree,
                       idTree,
                       depth,
                       parse_function,
                       tabset_type = "tabset tabset-pills"){
  
  for(i in 1:length(listTree)){
    if(class(listTree[[i]])[1] == "list"){
      
      cat(paste0("<div id='",names(idTree)[i],"' class='section level",depth," ",tabset_type,"'>"))
      cat(paste0("<h",depth,">",
                 str_replace_all(names(listTree)[i],pattern = "__"," "),
                 "</h",depth,">"))
      
      next_depth = depth+1
      nest_tabset(listTree = listTree[[i]],
                  idTree = idTree[[i]],
                  depth = next_depth,
                  parse_function = parse_function,
                  tabset_type = tabset_type)
    
    }else{
      
      cat(paste0("<div id='",idTree[[i]],"' class='section level",depth,"'>"))
      cat(paste0("<h",depth,">",
                 str_replace_all(names(listTree)[i],pattern = "__"," "),
                 "<br></h",depth,">"))
      
      parse_function(listTree[[i]])
      
      cat('\n',"<br>", '\n\n')
      cat("</div>")
      
    }
  }
}