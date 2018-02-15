##' Molecule Dictionary Query
##' 
##' Querys the molecule dictionary, mdq() is a shorthand wrapper fucntion
##' 
##' @param mol molecular formula of molecule to be queried
##' @param property property of the molecule to be returned not required for "molecules" or "structure
##' @param dictionary moleclar dictionary to use, defualt loaded by molecule_dictionary()
##' 
##' @return Corresponding value from molecule_dictionary
##' 
##' @author W S. Drysdale
##' 
##' @export


mol_dict_query = function(mol = NA,property = NA,dictionary = molecule_dictionary()){
  if(mol[1] == "all")
    mol = names(dictionary)[3:length(dictionary)]
  if(is.character(mol))
    mol = tolower(mol)
  if(!missing(property))
    property = tolower(property)
  #if one item queried
  if(length(mol) == 1 & length(property) == 1){
    if(mol %in% c("molecules","structure"))
      dictionary[mol][[1]]
    else{
      if(missing(property))
        dictionary[[mol]]
      else
        dictionary[[c(mol,property)]]
    }
  }else{
    #if many to many relationship is quried
    mol %<>% as.array
    property %<>% as.array
    if(length(mol) > 1 & length(property) > 1){
      for(i in 1:length(mol)){
        y = apply(property,1,function(x) dictionary[[c(mol[i],x)]])
        if(i == 1)
          z = data.frame(t(y))
        else
          z = rbind(z,data.frame(t(y)))
      }
      names(z) = property
      z$mol = c(mol)
      #return
      z
    }else{
      if(length(mol) > length(property)){
        z = apply(mol,1,function(x) dictionary[[c(x,property)]])
        z = data.frame(mol,z)
        names(z)[2] = property
        }
      if(length(property) > length(mol)){
        z = apply(property,1,function(x) dictionary[[c(mol,x)]])
        z = data.frame(property,z)
        names(z)[2] = mol
        }
      #return
      z
    }
  }
}
##' Molecule Dictionary Query
##' 
##' Querys the molecule dictionary, mdq() is a shorthand wrapper fucntion
##' 
##' @param mol molecular formula of molecule to be queried
##' @param property property of the molecule to be returned not required for "molecules" or "structure
##' @param dictionary moleclar dictionary to use, defualt loaded by molecule_dictionary()
##' 
##' @return Corresponding value from molecule_dictionary
##' 
##' @author W S. Drysdale
##' 
##' @export


mdq = function(mol = NULL,property = NULL){
  mol_dict_query(mol,property,dictionary = molecule_dictionary())
}
