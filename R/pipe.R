## From https://github.com/rstudio/ggvis/blob/master/R/pipe.R

#' Pipe PSTricks Object
#'
#' Like `dplyr`, PSTricks also uses the pipe function, \code{\%>\%}, to pass
#' information from one function to another. But this is unlike `ggplot2`, which
#' uses the `+` operator.
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A PSTricks object and a function to apply to it.
#' @examples
#' # Instead of
#' geom_dots(PSTricks(),aes(x=wt,y=mpg),mtcars)
#' # one may write
#' PSTricks() %>% geom_dots(aes(x=wt,y=mpg),mtcars)
NULL
