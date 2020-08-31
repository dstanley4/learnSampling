#' This is the documentation for stat_histotext
#' @param mapping Set of aesthetic mappings created by aes() or aes_(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping
#' @param data The data to be displayed in this layer
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' @param ... Other arguments
#' @export
stat_histotext <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {

  list(
    ggplot2::layer(
      stat = Histotext, data = data, mapping = mapping, geom = "text",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, parse = TRUE,...)
    )
  )
}


Histotext <- ggplot2::ggproto("Histotext", ggplot2::Stat,
                                       required_aes = c("x"),
                                       compute_panel = function(data, scales, text = "O", binwidth = NULL, breaks = NULL, max_chars_per_column = 25) {
                                         data <- get_text_points(data, text, binwidth, breaks, max_chars_per_column)
                                         #print(data)
                                         data
                                       }
)



