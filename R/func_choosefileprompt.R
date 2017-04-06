#' Choose file prompt
#' 
#' when given a vector of file names, asks the user to select the number of which file they desire to load
#' 
#' @param flist vector of fiel names
#' 

choosefileprompt = function(flist){
  j = readline(prompt = "Choose file number: ")
  if (j >= 1 & j <= length(flist))
    return(j)
  else
    print("Please Choose Valid filenumber")
    choosefileprompt
}