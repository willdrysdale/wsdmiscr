#' Legend Theme
#' 
#' Basic formatting for ggplot legends
#' 
#' @param fill_col controls plot background colour,default grey97
#' @param line_col controls plot line colour, default black
#' @param pos Legend Postion, default right
#' 
#' @export

leg_theme = function(fill_col = "grey97",line_col = "black",pos = "right"){
  theme(
    legend.position = pos,
    legend.text = element_text(colour = line_col,size = 12),
    legend.title = element_text(colour = line_col,size = 15),
    legend.key = element_blank(),
    legend.background = element_rect(fill = fill_col, colour = fill_col)
  )
}
