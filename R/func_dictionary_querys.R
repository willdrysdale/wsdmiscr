#' Molecule Dictionary Query
#' 
#' Querys the molecule dictionary
#' 
#' @param mol molecular formula of molecule to be queried
#' @param property property of the molecule to be returned not required for "molecules" or "structure
#' @param dictionary moleclar dictionary to use, defualt loaded by molecule_dictionary()
#' 
#' @return Corresponding value from molecule_dictionary
#' 
#' @author W S. Drysdale
#' 
#' @export


mol_dict_query = function(mol,property,dictionary = molecule_dictionary()){
  if(is.character(mol))
    mol = tolower(mol)
  if(!missing(property))
    property = tolower(property)
  if(mol %in% c("molecules","structure"))
    dictionary[mol][[1]]
  else{
    if(missing(property))
      dictionary[[mol]]
    else
      dictionary[[mol]][property][[1]]
  }
    
}