## 26 Axes

#' Draw PSTricks Axes
#' @param p The PSTricks object.
#' @param x,y Coordinates of the axes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-plot"),4,3,par="showgrid=true") %>%
#'     psaxes(c(2,0,4), c(1,0,3),
#'         "linewidth=1.2pt,labels=none,ticks=none", "<->")
#' # observe interesting showgrid

psaxes <- function(p=NULL, x, y, par=NULL, arrows=NULL)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, p=p)
}

## Perhaps a user would be interesting in using the original
## psaxes, but otherwise not in the data plotting commands, because
## these are easily handled in R, and these are therefore not layered
