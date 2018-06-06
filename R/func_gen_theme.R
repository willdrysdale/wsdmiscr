#' General Theme
#' 
#' General theme for use with ggplot
#' 
#' @param fill_col controls plot background colour,default grey97
#' @param line_col controls plot line colour, default black
#' 
#' @export


gen_theme = function(fill_col = "grey97",line_col = "black"){
  ggplot2::theme(
    plot.background = element_rect(fill = fill_col, colour = "grey92"),
    panel.background = element_rect(fill = fill_col, colour = fill_col),
    panel.grid.major = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(colour = line_col,size = 25,hjust = 0.5),
    axis.text = element_text(colour = line_col,size = 20,face = "bold"),
    axis.title = element_text(colour = line_col,size = 25),
    axis.ticks = element_line(colour = line_col),
    axis.line = element_line(colour = line_col),
    panel.border = element_rect(colour = line_col,fill = NA)
  )
}
