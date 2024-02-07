#' Draw Grid Lines
#' @details
#' Axes should be drawn before a grid.
#' Issue: with "linestyle=dotted" multiple dots are drawn at identical locations.
#' @param p The PSTricks object.
#' @param par PSTricks parameters.
#' @param background The optional background color.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),16,9) %>%
#'     newrgbcolor("verylightgray",.9,.9,.9) %>%
#'     ppsetmargins(mrgaxes=0) %>%
#'     ppaxis('x',c(0,1)) %>%
#'     ppaxis('y',c(0,1)) %>%
#'     ppgrid("linestyle=dotted,linecolor=gray",background="verylightgray")

ppgrid <- function(p, par="linestyle=dotted", background=NULL)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    x <- p$xtpos
    y <- p$ytpos

    if (is.null(x) || is.null(y)) stop("draw axes first")

    nx <- length(x)
    ny <- length(y)

    x0y0x <- adjx0y0(p, 'x', p$secondx)
    x0y0y <- adjx0y0(p, 'y', p$secondy)

    xf <- c(x0y0x[1], x0y0x[1]+p$hx)
    yf <- c(x0y0y[2], x0y0y[2]+p$hy)

    d2 <- p$linewidth
    d <- d2 / 2

    if (!is.null(background)) {
        p <- psframe(p, c(xf[1]+d,xf[2]-d), c(yf[1]+d,yf[2]-d),
                     paste0("linecolor=",background), star=TRUE)
    }

    if (isTRUE(all.equal(xf[1],x[1]))) x[1] <- x[1] + d2
    if (isTRUE(all.equal(xf[2],x[nx]))) x[nx] <- x[nx] - d2
    if (isTRUE(all.equal(yf[1],y[1]))) y[1] <- y[1] + d2
    if (isTRUE(all.equal(yf[2],y[ny]))) y[ny] <- y[ny] - d2

    for (i in 1:nx) {
        if (yf[1] < y[1]) {
            p <- psline(p, c(x[i],x[i]), c(yf[1]+d,y[1]+d), par)
        }
        for (j in 1:(ny-1)) {
            p <- psline(p, c(x[i],x[i]), c(y[j]-d,y[j+1]+d), par)
        }
        if (yf[2] > y[ny]) {
            p <- psline(p, c(x[i],x[i]), c(y[ny]-d,yf[2]-d), par)
        }
    }
    for (j in 1:ny) {
        if (xf[1] < x[1]) {
            p <- psline(p, c(xf[1]+d,x[1]+d), c(y[j],y[j]), par)
        }
        for (i in 1:(nx-1)) {
            p <- psline(p, c(x[i]-d,x[i+1]+d), c(y[j],y[j]), par)
        }
        if (xf[2] > x[nx]) {
            p <- psline(p, c(x[nx]-d,xf[2]-d), c(y[j],y[j]), par)
        }
    }

    p
}
