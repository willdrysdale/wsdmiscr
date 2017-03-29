#' Save File Prompt
#' 
#' Manages the top level flow when saving the file from btnox_adddata()
#' 
#' @param d dataframe being saved
#' @param fn filename determined by ncas_filename()
#' 
#' @return NULL

savefileprompt = function(d,fn){
  u = readline(prompt = "Write the series to a new file? [y/n]")
  if (u == "y" | u == "n"){
    if (u == "y")
      workingdirectoryprompt(d,fn)
    if (u == "n")
      return(NULL)
  }else
    savefileprompt(d,fn)
}