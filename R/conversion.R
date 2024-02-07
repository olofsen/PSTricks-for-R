#' Clip Data to Axes Space
#' @details Used internally by `cx` and `cy`.
#' @param x Data.
#' @param lim Limits.
#' @return Updated x data.
#' @noRd

clip <- function(x, lim)
{
    x[x<lim[1]] <- lim[1]
    x[x>lim[2]] <- lim[2]
    x
}

#' Convert Unscaled `x` Values to Scaled
#' @param p The PSTricks object.
#' @param x Unscaled data.
#' @param logx Flag to request log(10) transformation.
#' @return Scaled data.
#' @export

cx <- function(p, x, logx=NULL)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    if (is.null(logx)) logx <- p$logx
    if (logx) x <- log10(x)
    if (p$picpar$star) {
        return(ifelse(is.infinite(x),
                      clip(x, c(p$x0+1.5*p$margin+p$linewidth,p$x0+p$hx+2.5*p$margin-p$linewidth)),
                      x*p$xa+p$xb))
    }
    clip(x*p$xa+p$xb,
         c(p$x0+1.5*p$margin+p$linewidth,p$x0+p$hx+2.5*p$margin-p$linewidth))
}

#' Convert Unscaled `y` Values to Scaled
#' @param p The PSTricks object.
#' @param y Unscaled data.
#' @param logy Flag to request log(10) transformation.
#' @return Scaled data.
#' @export

cy <- function(p, y, logy=NULL)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    if (is.null(logy)) logy <- p$logy
    if (logy) y <- log10(y)
    if (p$picpar$star) {
        return(ifelse(is.infinite(y),
                      clip(y, c(p$y0+1.5*p$margin+p$linewidth,p$y0+p$hy+2.5*p$margin-p$linewidth)),
                      y*p$ya + p$yb))

    }
    clip(y*p$ya+p$yb,
         c(p$y0+1.5*p$margin+p$linewidth,p$y0+p$hy+2.5*p$margin-p$linewidth))
}

#' Convert Scaled `x` Values to Unscaled
#' @param p The PSTricks object.
#' @param x Scaled data.
#' @param logx Flag to request log(10) transformation.
#' @return Unscaled data.
#' @export

icx <- function(p, x, logx=NULL)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    if (is.null(logx)) logx <- p$logx
    x <- (x - p$xb) / p$xa
    if (logx) x <- 10^x
    x
}

#' Convert Scaled `y` Values to Unscaled
#' @param p The PSTricks object.
#' @param y Scaled data.
#' @param logy Flag to request log(10) transformation.
#' @return Unscaled data.
#' @export

icy <- function(p, y, logy=NULL)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    if (is.null(logy)) logy <- p$logy
    y <- (y - p$yb) / p$ya
    if (logy) y <- 10^y
    y
}
