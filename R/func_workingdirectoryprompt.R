#' Working Directory Prompt
#' 
#' Manages the bottom level flow when saving the file from btnox_adddata()
#' 
#' @param d dataframe being saved
#' @param fn filename determined by ncas_filename()
#' 
#' @return NULL

workingdirectoryprompt = function(d,fn){
  i = readline(prompt = paste("File will be saved in ",getwd(),"? Confirm [y/n]"))
  if (i == "y" | i == "n"){
    if (i == "y")
      write.csv(d,file = fn,row.names = F)
    else{
      if (i == "n"){
        writeLines("Please select desired working directory and continue")
        savefileprompt(d,fn)
      }
    }
  }else
    workingdirectoryprompt(d,fn)
}