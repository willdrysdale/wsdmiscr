#' Molecule Dictionary
#' 
#' List of molecules with information about them stored in a query-able fashion
#' mw for molecular weight
#' mw_unit returns the until of the molecular weight
#' parse_str string that can be passsed to parse(text = ) for correct formatting
#' name full name of the molecule
#' comment any commments about the entry
#' ... additional queryable items listed in comment
#' 
#' @param none
#' 
#' @return list containg the dictionary
#' 
#' @export
#' 
#' @author W S. Drysdale 

molecule_dictionary = function(){
  molecules = list(
    molecules = c("no","no2","o3","nox","so2","co","ch4","nh3","hono"),
    structure = c("mw - numeric molecular weight",
                  "mw_unit - unit of moecular weight",
                  "parse_str - string for parse(text = )",
                  "name - written name of molecule",
                  "comment - notes about entry",
                  "... - properties specifically for this species describe in comment"),
    
    no = list(
      mw = 30.0061,
      mw_unit = "g/mol",
      parse_str = "NO",
      name = "Nitric Oxide",
      comment = NULL
    ),
    
    no2 = list(
      mw = 46.0055,
      mw_unit = "g/mol",
      parse_str = "NO[2]",
      name = "Nitrogen Dioxide",
      comment = NULL
    ),
    
    o3 = list(
      mw = 47.9982,
      mw_unit = "g/mol",
      parse_str = "O[3]",
      name = "Ozone",
      comment = NULL
    ),
    
    nox = list(
      mw = 39.00215,
      mw_unit = "g/mol",
      parse_str = "NO[x]",
      name = "Oxides of Nitrogen",
      comment = "Molecular Weight given as average of NO and NO2"
    ),
    
    so2 = list(
      mw =  64.0638,
      mw_unit = "g/mol",
      parse_str = "SO[2]",
      name = "Sulphur Dioxide",
      comment = "name2 includes American spelling",
      name2 = "Sulfur Dioxide"
    ),
    
    co = list(
      mw =  28.0101,
      mw_unit = "g/mol",
      parse_str = "CO",
      name = "Carbon Monoxide",
      comment = NULL
    ),
    
    ch4 = list(
      mw = 16.0425,
      mw_unit = "g/mol",
      parse_str = "CH[4]",
      name = "Methane",
      comment = NULL
    ),
    
    nh4 = list(
      mw = 18.0385,
      mw_unit = "g/mol",
      parse_str = "NH[4]",
      name = "Ammonia",
      comment = NULL
    ),
    
    hono = list(
      mw = 47.01344,
      mw_unit = "g/mol",
      parse_str = "HONO",
      name = "Nitrous Acid",
      comment = NULL
    )
    
    
  )
  
  
  return(molecules)
}