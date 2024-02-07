#' Calculate Origin of Axis based on Origin of Subplot
#' @param p The PSTricks object.
#' @param xory A character 'x' or 'y' designating the axis.
#' @param secondary A flag to designate a secondary axis.
#' @return An origin.
#' @export

adjx0y0 <- function(p, xory, secondary)
{
    x0 <- p$x0
    y0 <- p$y0

    if (xory == 'x') {
        if (secondary) {
            x0 <- x0 + p$margin*(1.5 + 0.5*p$mrgaxes)
            y0 <- y0 + p$margin*2 + p$hy + p$margin/2
        } else {
            x0 <- x0 + p$margin*(1.5 + 0.5*p$mrgaxes)
            y0 <- y0 + p$margin*1.5
        }
    } else if (xory == 'y') {
        if (secondary) {
            x0 <- x0 + p$margin*2 + p$hx + p$margin/2
            y0 <- y0 + p$margin*(1.5 + 0.5*p$mrgaxes)
        } else {
            x0 <- x0 + p$margin*1.5
            y0 <- y0 + p$margin*(1.5 + 0.5*p$mrgaxes)
        }
    }

    c(x0,y0)
}
