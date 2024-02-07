#' Set Plot Title
#' @details The title is shown using `uput()`.
#' @param p The PSTricks object.
#' @param title The title.
#' @param dx,dy Offset with respect to the default position (top left).
#' @return The updated PSTricks object.
#' @seealso [ppmansubplot()] for an example.
#' @export

pptitle <- function(p, title, dx=0, dy=0)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    uput(p, p$x0+p$margin*2+dx, p$y0+p$dy-p$margin*0.5+dy, title, 'r')
}

## no alias as title because that function exists in base graphics
## and therefore would give a warning when the current package is loaded
