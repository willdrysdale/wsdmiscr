#' create_nested_colours
#' 
#' Based on a dataframe with two grouping columns, return a named vector of colours for interactions of these groups.
#' The hues are spaced based on the primary_group, using \code{scales::hue_pal()}, and vary in saturation based
#' on the secondary_group, using \code{colortools::sequential()}
#' 
#' @param df data.frame 
#' @param primary_group name of column containting the primary group (character)
#' @param secondary_group name of column containing the secondary group (character)
#' @param primary_cols optional set the hues of the colours manually. must be as many hues as there are primary groups (character vector)
#' 
#' @author W. S. Drysdake
#' 
#' @export
#' 

create_nested_colours = function(df,primary_group,secondary_group,primary_cols = NULL){
  
  df = df[,c(primary_group,secondary_group)] %>% 
    dplyr::distinct()
  
  df_n = df %>% 
    dplyr::group_by(.data[[primary_group]]) %>% 
    dplyr::count()
  
  df = df %>% 
    mutate(total_grp = interaction(.data[[primary_group]],.data[[secondary_group]]))
  
  if(is.null(primary_cols)){
    primary_cols = scales::hue_pal()(length(unique(df[[primary_group]])))
  }else{
    if(length(primary_cols) != nrow(df_n))
      stop(paste0("Insufficient values in manual scale. ",nrow(df_n)," needed but only ",length(primary_cols)," provided"))
  }
  
  df_n = df_n %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(prim = primary_cols) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(pal = list(colortools::sequential(prim,1/(n*0.01),plot = F)[2:(n+1)]))
  
  
  df$pal = tidyr::unnest(df_n,pal)$pal
  
  output = df$pal
  
  names(output) = df$total_grp  
  
  # 
  output
}