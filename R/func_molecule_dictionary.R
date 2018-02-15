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
      parse_str = "Methane",
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
      parse_str = "Ethane",
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
      parse_str = "Propane",
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
      parse_str = "italic(iso)-Butane",
      name = 'iso_Butane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkane',
      oh_rate_const = '2.12e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nbutane = list(
      mw = 58.12,
      mw_unit = 'g mol-1',
      parse_str = "n-Butane",
      name = 'n_Butane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkane',
      oh_rate_const = '2.36e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    cyclopentane = list(
      mw = 70.14,
      mw_unit = 'g mol-1',
      parse_str = "Cyclopentane",
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
      parse_str = "italic(iso)-Pentane",
      name = 'iso_Pentane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '5',
      func_grp = 'alkane',
      oh_rate_const = '3.6e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    npentane = list(
      mw = 72.15,
      mw_unit = 'g mol-1',
      parse_str = "n-pentane",
      name = 'n_pentane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '5',
      func_grp = 'alkane',
      oh_rate_const = '3.8e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x23methylpentane = list(
      mw = 86.18,
      mw_unit = 'g mol-1',
      parse_str = "2+3-methyl-Pentane",
      name = '2+3_Methyl_Pentane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '5',
      func_grp = 'alkane',
      oh_rate_const = '5.2e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nhexane = list(
      mw = 86.18,
      mw_unit = 'g mol-1',
      parse_str = "n-Hexane",
      name = 'n_Hexane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '6',
      func_grp = 'alkane',
      oh_rate_const = '5.2e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c6group = list(
      mw = 86.18,
      mw_unit = 'g mol-1',
      parse_str = "C[6]-group",
      name = 'C6_group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '6',
      func_grp = 'alkane',
      oh_rate_const = '5.2e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nheptane = list(
      mw = 100.21,
      mw_unit = 'g mol-1',
      parse_str = "italic(n)-Heptane",
      name = 'n_Heptane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '7',
      func_grp = 'alkane',
      oh_rate_const = '6.76e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c7group = list(
      mw = 100.21,
      mw_unit = 'g mol-1',
      parse_str = "C[7]-group",
      name = 'C7_group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '7',
      func_grp = 'alkane',
      oh_rate_const = '6.76e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x224tmp = list(
      mw = 114.23,
      mw_unit = 'g mol-1',
      parse_str = expression("2"*","*"2"*","*"4-trimethyl-Pentane"),
      name = '2,2,4_Trimethyl_Pentane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '8',
      func_grp = 'alkane',
      oh_rate_const = '3.34e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    noctane = list(
      mw = 114.23,
      mw_unit = 'g mol-1',
      parse_str = "italic(n)-Octane",
      name = 'n_Octane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '8',
      func_grp = 'alkane',
      oh_rate_const = '8.11e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c8group = list(
      mw = 114.23,
      mw_unit = 'g mol-1',
      parse_str = "C[8]-group",
      name = 'C8_group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '8',
      func_grp = 'alkane',
      oh_rate_const = '8.11e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nnonane = list(
      mw = 128.26,
      mw_unit = 'g mol-1',
      parse_str = "italic(n)-Nonane",
      name = 'n_Nonane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'alkane',
      oh_rate_const = '9.7e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c9group = list(
      mw = 128.26,
      mw_unit = 'g mol-1',
      parse_str = "C[9]-group",
      name = 'C9_group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'alkane',
      oh_rate_const = '9.7e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    ndecane = list(
      mw = 142.29,
      mw_unit = 'g mol-1',
      parse_str = "italic(n)-Decane",
      name = 'n_Decane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '10',
      func_grp = 'alkane',
      oh_rate_const = '1.1e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c10group = list(
      mw = 142.29,
      mw_unit = 'g mol-1',
      parse_str = "C[10]-group",
      name = 'C10_group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '10',
      func_grp = 'alkane',
      oh_rate_const = '1.1e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nundecane = list(
      mw = 156.31,
      mw_unit = 'g mol-1',
      parse_str = "italic(n)-Undecane",
      name = 'n_Undecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '11',
      func_grp = 'alkane',
      oh_rate_const = '1.23e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c11group = list(
      mw = 156.31,
      mw_unit = 'g mol-1',
      parse_str = "C[11]-group",
      name = 'C11_group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '11',
      func_grp = 'alkane',
      oh_rate_const = '1.23e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    ndodecane = list(
      mw = 170.34,
      mw_unit = 'g mol-1',
      parse_str = "italic(n)-Dodecane",
      name = 'n_Dodecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '12',
      func_grp = 'alkane',
      oh_rate_const = '1.32e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c12group = list(
      mw = 170.34,
      mw_unit = 'g mol-1',
      parse_str = "C[12]-group",
      name = 'C12_group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '12',
      func_grp = 'alkane',
      oh_rate_const = '1.32e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    ntridecane = list(
      mw = 184.37,
      mw_unit = 'g mol-1',
      parse_str = "italic(n)-Tridecane",
      name = 'n_Tridecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '13',
      func_grp = 'alkane',
      oh_rate_const = '1.51e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c13group = list(
      mw = 184.37,
      mw_unit = 'g mol-1',
      parse_str = "C[13]-group",
      name = 'C13_group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '13',
      func_grp = 'alkane',
      oh_rate_const = '1.51e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    ntetradecane = list(
      mw = 198.39,
      mw_unit = 'g mol-1',
      parse_str = "italic(n)-Tetradecane",
      name = 'n_Tetradecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '14',
      func_grp = 'alkane',
      oh_rate_const = '1.79e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c14group = list(
      mw = 198.39,
      mw_unit = 'g mol-1',
      parse_str = "C[14]-group",
      name = 'C14_group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '14',
      func_grp = 'alkane',
      oh_rate_const = '1.79e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    npentadecane = list(
      mw = 212.42,
      mw_unit = 'g mol-1',
      parse_str = "italic(n)-Pentadecane",
      name = 'n_Pentadecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '15',
      func_grp = 'alkane',
      oh_rate_const = '2.07e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c15group = list(
      mw = 212.42,
      mw_unit = 'g mol-1',
      parse_str = "C[15]-group",
      name = 'C15_group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '15',
      func_grp = 'alkane',
      oh_rate_const = '2.07e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nhexadecane = list(
      mw = 226.41,
      mw_unit = 'g mol-1',
      parse_str = "italic(n)-Hexadecane",
      name = 'n_Hexadecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '16',
      func_grp = 'alkane',
      oh_rate_const = '2.32e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c16group = list(
      mw = 226.41,
      mw_unit = 'g mol-1',
      parse_str = "C[16]-group",
      name = 'C16_group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '16',
      func_grp = 'alkane',
      oh_rate_const = '2.32e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nheptadecane = list(
      mw = 240.48,
      mw_unit = 'g mol-1',
      parse_str = "italic(n)-Heptadecane",
      name = 'n_Heptadecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '17',
      func_grp = 'alkane',
      oh_rate_const = 'NA',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c17group = list(
      mw = 240.48,
      mw_unit = 'g mol-1',
      parse_str = "C[17]-group",
      name = 'C17_group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '17',
      func_grp = 'alkane',
      oh_rate_const = 'NA',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    noctadecane = list(
      mw = 254.5,
      mw_unit = 'g mol-1',
      parse_str = "italic(n)-Octadecane",
      name = 'n_Octadecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '18',
      func_grp = 'alkane',
      oh_rate_const = 'NA',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c18group = list(
      mw = 254.5,
      mw_unit = 'g mol-1',
      parse_str = "C[18]-group",
      name = 'C18_group',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '18',
      func_grp = 'alkane',
      oh_rate_const = 'NA',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    nnonadecane = list(
      mw = 268.5227,
      mw_unit = 'g mol-1',
      parse_str = "italic(n)-Nonadecane",
      name = 'n_Nonadecane',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '19',
      func_grp = 'alkane',
      oh_rate_const = 'NA',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    ethene = list(
      mw = 28.05,
      mw_unit = 'g mol-1',
      parse_str = "Ethene",
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
      parse_str = "Acetylene",
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
      parse_str = "Propene",
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
      parse_str = "Propadiene",
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
      parse_str = "Propyne",
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
      parse_str = "italic(trans)-2-Butene",
      name = 'trans_2_Butene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkene',
      oh_rate_const = '6.4e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x1butene = list(
      mw = 56.11,
      mw_unit = 'g mol-1',
      parse_str = "1-Butene",
      name = '1_Butene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkene',
      oh_rate_const = '3.14e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    isobutene = list(
      mw = 56.11,
      mw_unit = 'g mol-1',
      parse_str = "italic(iso)-Butene",
      name = 'iso_butene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkene',
      oh_rate_const = '5.14e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c2butene = list(
      mw = 56.11,
      mw_unit = 'g mol-1',
      parse_str = "C[2]-Butene",
      name = 'C2_Butene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkene',
      oh_rate_const = '5.64e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x12butadiene = list(
      mw = 54.09,
      mw_unit = 'g mol-1',
      parse_str = expression("1"*","*"2-Butadiene"),
      name = '1_2_Butadiene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkene',
      oh_rate_const = '2.69e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x13butadiene = list(
      mw = 54.09,
      mw_unit = 'g mol-1',
      parse_str = expression("1"*","*"3-Butadiene"),
      name = '1_3_Butadiene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '4',
      func_grp = 'alkene',
      oh_rate_const = '6.66e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    t2pentene = list(
      mw = 70.14,
      mw_unit = 'g mol-1',
      parse_str = "italic(trans)-2-Pentene",
      name = 'trans_2_Pentene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '5',
      func_grp = 'alkene',
      oh_rate_const = '6.7e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x1pentene = list(
      mw = 70.14,
      mw_unit = 'g mol-1',
      parse_str = "1-Pentene",
      name = '1_Pentene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '5',
      func_grp = 'alkene',
      oh_rate_const = '3.14e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    isoprene = list(
      mw = 68.12,
      mw_unit = 'g mol-1',
      parse_str = "Isoprene",
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
      parse_str = "Styrene",
      name = 'Styrene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '8',
      func_grp = 'alkene',
      oh_rate_const = '5.8e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    apinene = list(
      mw = 136.24,
      mw_unit = 'g mol-1',
      parse_str = paste0(expression(alpha),"-pinene"),
      name = 'alpha_Pinene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '10',
      func_grp = 'alkene',
      oh_rate_const = '5.23e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    limonene = list(
      mw = 136.24,
      mw_unit = 'g mol-1',
      parse_str = "Limonene",
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
      parse_str = "Monoterpenes",
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
      parse_str = "Benzene",
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
      parse_str = "Toluene",
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
      parse_str = "EthylBenzene",
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
      parse_str = "italic(m)-~and~italic(p)-~Xylene",
      name = 'm_p_Xylene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '8',
      func_grp = 'aromatic',
      oh_rate_const = '1.87e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    oxylene = list(
      mw = 106.17,
      mw_unit = 'g mol-1',
      parse_str = "italic(o)-xylene",
      name = 'o_xylene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '8',
      func_grp = 'aromatic',
      oh_rate_const = '1.36e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    isopropylbenzene = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = "italic(iso)-propyl-Benzene",
      name = 'Iso_propyl_benzene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '6.3e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    propylbenzene = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = "propyl-Benzene",
      name = 'propyl_benzene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '5.8e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x3ethyltoluene = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = "3-ethyl-Toluene",
      name = '3_ethyl_toluene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '1.86e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x4ethyltoluene = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = "3-ethyl-Toluene",
      name = '3_ ethyl_toluene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '1.18e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x135tmb = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = expression("1"*","*"3"*","*"5-trimethyl Benzene"),
      name = '1_3_5_trimethyl_Benzene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '5.67e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x2ethyltoluene = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = "2_ethyl_Toluene",
      name = '2_ethyl_Toluene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '1.19e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x124tmb = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = expression("1"*","*"2"*","*"4-trimethyl Benzene"),
      name = '1_2_4_trimethyl_Benzene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '3.25e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    x123tmb = list(
      mw = 120.19,
      mw_unit = 'g mol-1',
      parse_str = expression("1"*","*"2"*","*"5-trimethyl Benzene"),
      name = '1_2_3_trimethyl_Benzene',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '9',
      func_grp = 'aromatic',
      oh_rate_const = '3.27e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    c4monoarogroup = list(
      mw = 134.22,
      mw_unit = 'g mol-1',
      parse_str = "C[4]-Monoaromatics",
      name = 'C4_Monoaromatics',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '10',
      func_grp = 'aromatic',
      oh_rate_const = '1.12e-11',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    methanol = list(
      mw = 32.04,
      mw_unit = 'g mol-1',
      parse_str = "Methanol",
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
      parse_str = "Acetaldehyde",
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
      parse_str = "Acetone",
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
      parse_str = "Ethanol",
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
      parse_str = "Propanol",
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
      parse_str = "Butanol",
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
      parse_str = "2-Hexanone",
      name = '2_Hexanone',
      comment = 'VOC - c_num, func_grp, oh_rate_const and oh_rate_const_unit avaliable',
      c_num = '6',
      func_grp = 'oxygenated',
      oh_rate_const = '9.1e-12',
      oh_rate_const_unit = 'cm3 molecule-1 s-1'
    ),
    
    hexanal = list(
      mw = 100.16,
      mw_unit = 'g mol-1',
      parse_str = "Hexanol",
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