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
    molecules = "",
    structure = c("mw - numeric molecular weight",
                  "mw_unit - unit of moecular weight",
                  "parse_str - string for parse(text = )",
                  "name - written name of molecule",
                  "comment - notes about entry",
                  "c_num - carbon number, NA if undefined",
                  "func_grp - functional group, NA if undefined",
                  "oh_rate_const - OH rate constant, NA if undefined",
                  "oh_rate_const_unit - OH rate constant unit, NA if undefined",
                  "... - properties specifically for this species describe in comment"),
    
    no = list(
      mw = 30.0061,
      mw_unit = "g mol-1",
      parse_str = "NO",
      name = "Nitric Oxide",
      comment = NULL,
      c_num = "",
      func_grp = "",
      oh_rate_const = "",
      oh_rate_const_unit = ""
    ),
    
    no2 = list(
      mw = 46.0055,
      mw_unit = "g mol-1",
      parse_str = "NO[2]",
      name = "Nitrogen Dioxide",
      comment = NULL,
      c_num = "",
      func_grp = "",
      oh_rate_const = "",
      oh_rate_const_unit = ""
    ),
    
    o3 = list(
      mw = 47.9982,
      mw_unit = "g mol-1",
      parse_str = "O[3]",
      name = "Ozone",
      comment = NULL,
      c_num = "",
      func_grp = "",
      oh_rate_const = "",
      oh_rate_const_unit = ""
    ),
    
    nox = list(
      mw = 39.00215,
      mw_unit = "g mol-1",
      parse_str = "NO[x]",
      name = "Oxides of Nitrogen",
      comment = "Molecular Weight given as average of NO and NO2",
      c_num = "",
      func_grp = "",
      oh_rate_const = "",
      oh_rate_const_unit = ""
    ),
    
    so2 = list(
      mw =  64.0638,
      mw_unit = "g mol-1",
      parse_str = "SO[2]",
      name = "Sulphur Dioxide",
      comment = "name2 includes American spelling",
      name2 = "Sulfur Dioxide",
      c_num = "",
      func_grp = "",
      oh_rate_const = "",
      oh_rate_const_unit = ""
    ),
    
    co = list(
      mw =  28.0101,
      mw_unit = "g mol-1",
      parse_str = "CO",
      name = "Carbon Monoxide",
      comment = NULL,
      c_num = "",
      func_grp = "",
      oh_rate_const = "",
      oh_rate_const_unit = ""
    ),
    
    ch4 = list(
      mw = 16.0425,
      mw_unit = "g mol-1",
      parse_str = "CH[4]",
      name = "Methane",
      comment = NULL,
      c_num = "",
      func_grp = "",
      oh_rate_const = "",
      oh_rate_const_unit = ""
    ),
    
    nh4 = list(
      mw = 18.0385,
      mw_unit = "g mol-1",
      parse_str = "NH[4]",
      name = "Ammonia",
      comment = NULL,
      c_num = "",
      func_grp = "",
      oh_rate_const = "",
      oh_rate_const_unit = ""
    ),
    
    hono = list(
      mw = 47.01344,
      mw_unit = "g mol-1",
      parse_str = "HONO",
      name = "Nitrous Acid",
      comment = NULL,
      c_num = "",
      func_grp = "",
      oh_rate_const = "",
      oh_rate_const_unit = ""
    ),
    methane = list(
      mw = 16.04,
      mw_unit = 'g mol-1',
      parse_str = 'Methane',
      name = 'Methane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '1',
      func_grp = 'alkane',
      oh_rate_const = '6.4e-15',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    ethane = list(
      mw = 30.069,
      mw_unit = 'g mol-1',
      parse_str = 'Ethane',
      name = 'Ethane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '2',
      func_grp = 'alkane',
      oh_rate_const = '2.48e-13',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    propane = list(
      mw = 44.1,
      mw_unit = 'g mol-1',
      parse_str = 'Propane',
      name = 'Propane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '3',
      func_grp = 'alkane',
      oh_rate_const = '1.09e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    isobutane = list(
      mw = 58.12,
      mw_unit = 'g mol-1',
      parse_str = 'Isobutane',
      name = 'Isobutane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkane',
      oh_rate_const = '2.12e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nbutane = list(
      mw = 58.12,
      mw_unit = 'g mol-1',
      parse_str = 'n-Butane',
      name = 'n-Butane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkane',
      oh_rate_const = '2.36e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    cyclopentane = list(
      mw = 70.14,
      mw_unit = 'g mol-1',
      parse_str = 'Cyclopentane',
      name = 'Cyclopentane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '5',
      func_grp = 'alkane',
      oh_rate_const = '4.97e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    isopentane = list(
      mw = 72.15,
      mw_unit = 'g mol-1',
      parse_str = 'Isopentane',
      name = 'Isopentane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '5',
      func_grp = 'alkane',
      oh_rate_const = '3.6e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    npentane = list(
      mw = 72.15,
      mw_unit = 'g mol-1',
      parse_str = 'n-pentane',
      name = 'n-pentane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '5',
      func_grp = 'alkane',
      oh_rate_const = '3.8e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x23methylpentane = list(
      mw = 86.18,
      mw_unit = 'g mol-1',
      parse_str = '2-3-Methylpentane',
      name = '2-3-Methylpentane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '5',
      func_grp = 'alkane',
      oh_rate_const = '5.2e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nhexane = list(
      mw = 86.18,
      mw_unit = 'g mol-1',
      parse_str = 'n-Hexane',
      name = 'n-Hexane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '6',
      func_grp = 'alkane',
      oh_rate_const = '5.2e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c6group = list(
      mw = 86.18,
      mw_unit = 'g mol-1',
      parse_str = 'C6-group',
      name = 'C6-group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '6',
      func_grp = 'alkane',
      oh_rate_const = '5.2e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nheptane = list(
      mw = 100.21,
      mw_unit = 'g mol-1',
      parse_str = 'n-Heptane',
      name = 'n-Heptane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '7',
      func_grp = 'alkane',
      oh_rate_const = '6.76e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c7group = list(
      mw = 100.21,
      mw_unit = 'g mol-1',
      parse_str = 'C7-group',
      name = 'C7-group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '7',
      func_grp = 'alkane',
      oh_rate_const = '6.76e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x224tmp = list(
      mw = 114.23,
      mw_unit = 'g mol-1',
      parse_str = '2-2-4 Trimethylpentane',
      name = '2-2-4 Trimethylpentane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '8',
      func_grp = 'alkane',
      oh_rate_const = '3.34e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    noctane = list(
      mw = 114.23,
      mw_unit = 'g mol-1',
      parse_str = 'n-Octane',
      name = 'n-Octane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '8',
      func_grp = 'alkane',
      oh_rate_const = '8.11e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c8group = list(
      mw = 114.23,
      mw_unit = 'g mol-1',
      parse_str = 'C8-group',
      name = 'C8-group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '8',
      func_grp = 'alkane',
      oh_rate_const = '8.11e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nnonane = list(
      mw = 128.26,
      mw_unit = 'g mol-1',
      parse_str = 'n-Nonane',
      name = 'n-Nonane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'alkane',
      oh_rate_const = '9.7e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c9group = list(
      mw = 128.26,
      mw_unit = 'g mol-1',
      parse_str = 'C9-group',
      name = 'C9-group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'alkane',
      oh_rate_const = '9.7e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    ndecane = list(
      mw = 142.29,
      mw_unit = 'g mol-1',
      parse_str = 'n-Decane',
      name = 'n-Decane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '10',
      func_grp = 'alkane',
      oh_rate_const = '1.1e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c10group = list(
      mw = 142.29,
      mw_unit = 'g mol-1',
      parse_str = 'C10-group',
      name = 'C10-group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '10',
      func_grp = 'alkane',
      oh_rate_const = '1.1e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nundecane = list(
      mw = 156.31,
      mw_unit = 'g mol-1',
      parse_str = 'n-Undecane',
      name = 'n-Undecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '11',
      func_grp = 'alkane',
      oh_rate_const = '1.23e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c11group = list(
      mw = 156.31,
      mw_unit = 'g mol-1',
      parse_str = 'C11-group',
      name = 'C11-group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '11',
      func_grp = 'alkane',
      oh_rate_const = '1.23e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    ndodecane = list(
      mw = 170.34,
      mw_unit = 'g mol-1',
      parse_str = 'n-Dodecane',
      name = 'n-Dodecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '12',
      func_grp = 'alkane',
      oh_rate_const = '1.32e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c12group = list(
      mw = 170.34,
      mw_unit = 'g mol-1',
      parse_str = 'C12-group',
      name = 'C12-group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '12',
      func_grp = 'alkane',
      oh_rate_const = '1.32e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    ntridecane = list(
      mw = 184.37,
      mw_unit = 'g mol-1',
      parse_str = 'n-Tridecane',
      name = 'n-Tridecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '13',
      func_grp = 'alkane',
      oh_rate_const = '1.51e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c13group = list(
      mw = 184.37,
      mw_unit = 'g mol-1',
      parse_str = 'C13-group',
      name = 'C13-group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '13',
      func_grp = 'alkane',
      oh_rate_const = '1.51e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    ntetradecane = list(
      mw = 198.39,
      mw_unit = 'g mol-1',
      parse_str = 'n-Tetradecane',
      name = 'n-Tetradecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '14',
      func_grp = 'alkane',
      oh_rate_const = '1.79e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c14group = list(
      mw = 198.39,
      mw_unit = 'g mol-1',
      parse_str = 'C14-group',
      name = 'C14-group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '14',
      func_grp = 'alkane',
      oh_rate_const = '1.79e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    npentadecane = list(
      mw = 212.42,
      mw_unit = 'g mol-1',
      parse_str = 'n-Pentadecane',
      name = 'n-Pentadecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '15',
      func_grp = 'alkane',
      oh_rate_const = '2.07e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c15group = list(
      mw = 212.42,
      mw_unit = 'g mol-1',
      parse_str = 'C15-group',
      name = 'C15-group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '15',
      func_grp = 'alkane',
      oh_rate_const = '2.07e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nhexadecane = list(
      mw = 226.41,
      mw_unit = 'g mol-1',
      parse_str = 'n-Hexadecane',
      name = 'n-Hexadecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '16',
      func_grp = 'alkane',
      oh_rate_const = '2.32e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c16group = list(
      mw = 226.41,
      mw_unit = 'g mol-1',
      parse_str = 'C16-group',
      name = 'C16-group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '16',
      func_grp = 'alkane',
      oh_rate_const = '2.32e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nheptadecane = list(
      mw = 240.48,
      mw_unit = 'g mol-1',
      parse_str = 'n-Heptadecane',
      name = 'n-Heptadecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '17',
      func_grp = 'alkane',
      oh_rate_const = 'NA',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c17group = list(
      mw = 240.48,
      mw_unit = 'g mol-1',
      parse_str = 'C17-group',
      name = 'C17-group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '17',
      func_grp = 'alkane',
      oh_rate_const = 'NA',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    noctadecane = list(
      mw = 254.5,
      mw_unit = 'g mol-1',
      parse_str = 'n-Octadecane',
      name = 'n-Octadecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '18',
      func_grp = 'alkane',
      oh_rate_const = 'NA',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c18group = list(
      mw = 254.5,
      mw_unit = 'g mol-1',
      parse_str = 'C18-group',
      name = 'C18-group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '18',
      func_grp = 'alkane',
      oh_rate_const = 'NA',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nnonadecane = list(
      mw = 268.5227,
      mw_unit = 'g mol-1',
      parse_str = 'n-Nonadecane',
      name = 'n-Nonadecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '19',
      func_grp = 'alkane',
      oh_rate_const = 'NA',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    ethene = list(
      mw = 28.05,
      mw_unit = 'g mol-1',
      parse_str = 'Ethene',
      name = 'Ethene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '2',
      func_grp = 'alkene',
      oh_rate_const = '8.52e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    acetylene = list(
      mw = 26.04,
      mw_unit = 'g mol-1',
      parse_str = 'Acetylene',
      name = 'Acetylene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '2',
      func_grp = 'alkene',
      oh_rate_const = '7.05e-13',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    propene = list(
      mw = 42.08,
      mw_unit = 'g mol-1',
      parse_str = 'Propene',
      name = 'Propene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '3',
      func_grp = 'alkene',
      oh_rate_const = '2.63e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    propadiene = list(
      mw = 40.06,
      mw_unit = 'g mol-1',
      parse_str = 'Propadiene',
      name = 'Propadiene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '3',
      func_grp = 'alkene',
      oh_rate_const = '9.65e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    propyne = list(
      mw = 40.06,
      mw_unit = 'g mol-1',
      parse_str = 'Propyne',
      name = 'Propyne',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '3',
      func_grp = 'alkene',
      oh_rate_const = '5.9e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    t2butene = list(
      mw = 56.11,
      mw_unit = 'g mol-1',
      parse_str = 'trans-But-2-ene',
      name = 'trans-But-2-ene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkene',
      oh_rate_const = '6.4e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x1butene = list(
      mw = 56.11,
      mw_unit = 'g mol-1',
      parse_str = 'But-1-ene',
      name = 'But-1-ene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkene',
      oh_rate_const = '3.14e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    isobutene = list(
      mw = 56.11,
      mw_unit = 'g mol-1',
      parse_str = 'Isobutene',
      name = 'Isobutene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkene',
      oh_rate_const = '5.14e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c2butene = list(
      mw = 56.11,
      mw_unit = 'g mol-1',
      parse_str = 'C2-Butene',
      name = 'C2-Butene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkene',
      oh_rate_const = '5.64e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x12butadiene = list(
      mw = 54.09,
      mw_unit = 'g mol-1',
      parse_str = 'Buta-1-2-diene',
      name = 'Buta-1-2-diene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkene',
      oh_rate_const = '2.69e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x13butadiene = list(
      mw = 54.09,
      mw_unit = 'g mol-1',
      parse_str = 'Buta-1-3-diene',
      name = 'Buta-1-3-diene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkene',
      oh_rate_const = '6.66e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    t2pentene = list(
      mw = 70.14,
      mw_unit = 'g mol-1',
      parse_str = 'trans-Pent-2-ene',
      name = 'trans-Pent-2-ene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '5',
      func_grp = 'alkene',
      oh_rate_const = '6.7e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x1pentene = list(
      mw = 70.14,
      mw_unit = 'g mol-1',
      parse_str = 'Pent-1ene',
      name = 'Pent-1ene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '5',
      func_grp = 'alkene',
      oh_rate_const = '3.14e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    isoprene = list(
      mw = 68.12,
      mw_unit = 'g mol-1',
      parse_str = 'Isoprene',
      name = 'Isoprene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '6',
      func_grp = 'alkene',
      oh_rate_const = '1e-10',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    stryene = list(
      mw = 104.15,
      mw_unit = 'g mol-1',
      parse_str = 'Syrene',
      name = 'Syrene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '8',
      func_grp = 'alkene',
      oh_rate_const = '5.8e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    apinene = list(
      mw = 136.24,
      mw_unit = 'g mol-1',
      parse_str = 'alpha-Pinene',
      name = 'alpha-Pinene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '10',
      func_grp = 'alkene',
      oh_rate_const = '5.23e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    limonene = list(
      mw = 136.24,
      mw_unit = 'g mol-1',
      parse_str = 'Limonene',
      name = 'Limonene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '10',
      func_grp = 'alkene',
      oh_rate_const = '1.64e-10',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    monoterps = list(
      mw = 136.24,
      mw_unit = 'g mol-1',
      parse_str = 'Monoterpenes',
      name = 'Monoterpenes',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '10',
      func_grp = 'alkene',
      oh_rate_const = '5.23e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    benzene = list(
      mw = 78.11,
      mw_unit = 'g mol-1',
      parse_str = 'Benzene',
      name = 'Benzene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '6',
      func_grp = 'aromatic',
      oh_rate_const = '1.22e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    toluene = list(
      mw = 92.14,
      mw_unit = 'g mol-1',
      parse_str = 'Toluene',
      name = 'Toluene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '7',
      func_grp = 'aromatic',
      oh_rate_const = '5.63e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    ethylbenzene = list(
      mw = 106.17,
      mw_unit = 'g mol-1',
      parse_str = 'EthylBenzene',
      name = 'EthylBenzene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '8',
      func_grp = 'aromatic',
      oh_rate_const = '7e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    mpxylene = list(
      mw = 106.17,
      mw_unit = 'g mol-1',
      parse_str = 'm-p-Xylene',
      name = 'm-p-Xylene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '8',
      func_grp = 'aromatic',
      oh_rate_const = '1.87e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    oxylene = list(
      mw = 106.17,
      mw_unit = 'g mol-1',
      parse_str = 'o-xylene',
      name = 'o-xylene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '8',
      func_grp = 'aromatic',
      oh_rate_const = '1.36e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    isopropylbenzene = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = 'Isopropylbenzene',
      name = 'Isopropylbenzene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '6.3e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    propylbenzene = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = 'Propylbenzene',
      name = 'Propylbenzene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '5.8e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x3ethyltoluene = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = '3 Ethyltoluene',
      name = '3 Ethyltoluene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '1.86e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x4ethyltoluene = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = '4 Ethyltoluene',
      name = '4 Ethyltoluene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '1.18e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x135tmb = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = '1-3-5 Trimethylbenzene',
      name = '1-3-5 Trimethylbenzene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '5.67e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x2ethyltoluene = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = '2 Ethyltoluene',
      name = '2 Ethyltoluene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '1.19e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x124tmb = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = '1-2-4 Trimethylbenzene',
      name = '1-2-4 Trimethylbenzene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '3.25e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x123tmb = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = '1-2-3 trimethylbenzene',
      name = '1-2-3 trimethylbenzene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '3.27e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c4monoarogroup = list(
      mw = 134.22,
      mw_unit = 'g mol-1',
      parse_str = 'C4-Monoaromatics',
      name = 'C4-Monoaromatics',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '10',
      func_grp = 'aromatic',
      oh_rate_const = '1.12e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    methanol = list(
      mw = 32.04,
      mw_unit = 'g mol-1',
      parse_str = 'Methanol',
      name = 'Methanol',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '1',
      func_grp = 'oxygenated',
      oh_rate_const = '9.4e-13',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    acetaldehyde = list(
      mw = 44.05,
      mw_unit = 'g mol-1',
      parse_str = 'Acetaldehyde',
      name = 'Acetaldehyde',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '2',
      func_grp = 'oxygenated',
      oh_rate_const = '1.5e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    acetone = list(
      mw = 58.08,
      mw_unit = 'g mol-1',
      parse_str = 'Acetone',
      name = 'Acetone',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '2',
      func_grp = 'oxygenated',
      oh_rate_const = '1.7e-13',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    ethanol = list(
      mw = 46.07,
      mw_unit = 'g mol-1',
      parse_str = 'Ethanol',
      name = 'Ethanol',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '2',
      func_grp = 'oxygenated',
      oh_rate_const = '3.2e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    propanol = list(
      mw = 60.1,
      mw_unit = 'g mol-1',
      parse_str = 'Propanol',
      name = 'Propanol',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '3',
      func_grp = 'oxygenated',
      oh_rate_const = '5.8e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    butanol = list(
      mw = 74.12,
      mw_unit = 'g mol-1',
      parse_str = 'Butanol',
      name = 'Butanol',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'oxygenated',
      oh_rate_const = '8.5e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x2hexanone = list(
      mw = 100.16,
      mw_unit = 'g mol-1',
      parse_str = '2-Hexanone',
      name = '2-Hexanone',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '6',
      func_grp = 'oxygenated',
      oh_rate_const = '9.1e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    hexanal = list(
      mw = 100.16,
      mw_unit = 'g mol-1',
      parse_str = 'Hexanol',
      name = 'Hexanol',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '6',
      func_grp = 'oxygenated',
      oh_rate_const = '3e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    )
  )
  molecules$molecules = names(molecules[3:length(molecules)])
  return(molecules)
}