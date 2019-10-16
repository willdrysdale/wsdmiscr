#' Loop Group
#' 
#' Take a data.frame with grouped data i.e grouped by hour for a diurnal, and duplicate \cr
#' the first group at the end of the data.frame, with its grouping variable == max(group)+1
#' 
#' @param df data.frame contained grouped/binned data
#' @param by value of the increment between groups, if NULL an attempt is made to guess this value
#' @param grp column containg grouping data. if NULL the first group of the grouped data.frame will be used.
#' 
#' @author W. S. Drysdale
#' 
#' @export

loop_group = function(df,by = NULL, grp = NULL){
  
  if(!class(by) %in% c("numeric","integer"))
    stop("by must be either numeric or integer")
  
  if(!class(data.frame(df)[,grp]) %in% c("numeric","integer"))
    stop("column that grp specifies must be numeric or integer")
  
  # if no grp is supplied, try to obtain from the data.frame's grouping info
  if(is.null(grp)){
    grp = dplyr::groups(df)[[1]] %>% 
      as.character()
    
    if(is.null(grp))
      stop("Unable to determine group. Please supply a grouped data.frame or specify the grouping column")
  }
  
  # If no by supplied, try to guess from the grp column
  if(is.null(by)){
    grp_seq = unique(data.frame(df)[,grp]) %>% 
      sort()
    
    if(grp_seq[1] == 0)
      grp_seq = grp_seq+grp_seq[2]
    
    by = max(grp_seq)/length(grp_seq)
    
  }
  
  wrap_df = df[df[,grp] == min(df[,grp],na.rm = T),]
  wrap_df[,grp] = max(df[,grp],na.rm = T) + by
  
  # return
  dplyr::bind_rows(df,wrap_df)
  
  
}