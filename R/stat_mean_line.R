##' @name stat_mean_line
##' 
##' @title Stat Mean Line
##' 
##' @rdname StatMeanLine
##' @description Plots a mean of all data in the facet in ggplot, taken from here https://stackoverflow.com/questions/46327431/
##' 
##' @export

StatMeanLine <- ggproto("StatMeanLine", Stat,
                        compute_group = function(data, scales) {
                          transform(data, yintercept=mean(y,na.rm = T))
                        },
                        required_aes = c("x", "y")
)
##' @rdname stat_mean_line
##' @title Stat Mean Line
##' @description Plots a mean of all data in the facet in ggplot, taken from here https://stackoverflow.com/questions/46327431/
##' @param x x
##' @param y y
##' @export

stat_mean_line <- function(mapping = NULL, data = NULL, geom = "hline",
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = TRUE, ...) {
  layer(
    stat = StatMeanLine, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
