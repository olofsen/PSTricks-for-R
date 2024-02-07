#' Add Legend to Plot
#' @param p The PSTricks object.
#' @param s The legend text.
#' @param par PSTricks parameter string.
#' @param position Position for the legend (may be NULL).
#' @param dx,dy x and y offsets w.r.t. default position.
#' @param w Width of the `psline()` that belongs to the legend text.
#' @param labelsep The distance between the line and the label.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' p <- pppicture(PSTricks(),16,9) %>%
#'     ppaxis('x',c(0,1)) %>%
#'     ppaxis('y',c(0,1)) ;
#' p <- p %>%
#'     psset("linecolor=green,showpoints=true") %>%
#'     psline(cx(p,seq(0,1,0.2)),cy(p,rep(0.5,5))) %>%
#'     pplegend("top right")

pplegend <- function(p, s, par=NULL, position='tr',
                     dx=0, dy=0, w=1, labelsep="10pt")
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    if (is.null(position)) {
        x <- p$x0 + dx
        y <- p$y0 + dy
        p <- psline(p, c(x,x+w/2,x+w), c(y,y,y), par=par)
        p <- uput(p, x+w, y, s, 'r', labelsep=labelsep)
    } else {
        d <- p$linewidth/2
        if (position == 'bl') {
            x <- dx + p$x0 + p$margin*(1.5 + 0.5*p$mrgaxes) + d
            y <- dy + p$y0 + p$margin*(1.5 + 0.5*p$mrgaxes) + d
        } else if (position == 'br') {
            x <- dx + p$x0 + p$hx + p$margin*(1.5 + 0.5*p$mrgaxes) - d
            y <- dy + p$y0 + p$margin*(1.5 + 0.5*p$mrgaxes) + d
        } else if (position == 'tl') {
            x <- dx + p$x0 + p$margin*(1.5 + 0.5*p$mrgaxes) + d
            y <- dy + p$y0 + p$hy + p$margin*(1.5 + 0.5*p$mrgaxes) - d
        } else if (position == 'tr') {
            x <- dx + p$x0 + p$hx + p$margin*(1.5 + 0.5*p$mrgaxes) - d
            y <- dy + p$y0 + p$hy + p$margin*(1.5 + 0.5*p$mrgaxes) - d
        } else {
            stop(paste0("unknown position `", position, "`"))
        }
        if (substr(position,2,2)=='l') {
            s <- paste0(psline(,c(0,w/2,w),c(0,0),par),uput(,w,0,s,'r'))
        } else {
            s <- paste0(psline(,c(0,-w/2,-w),c(0,0),par),uput(,-w,0,s,'l'))
        }
        p <- rput(p, x, y, s)
    }

    p
}
