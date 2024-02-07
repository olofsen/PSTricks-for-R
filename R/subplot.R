#' Divide the Picture in Subplots
#' @details
#' Subsequent coordinates are relative to (p$x0,p$y0),
#' so possibly different from (0,0).
#' Plot parameters such as limits, ticks, and labels are not reset to default values.
#' @param p The PSTricks object.
#' @param nx Number of plots in the x direction (if NULL, increment n automatically).
#' @param ny Number of plots in the y direction.
#' @param n Number of current plot (by default 1 if nx and ny specified).
#' @param nxaxes Number of x axes to make space for.
#' @param nyaxes Number of y axes to make space for.
#' @param ntitle Number of title lines to make space for.
#' @param width Number of subplots to occupy in the x direction.
#' @param height Number of subplots to occupy in the y direction.
#' @param newpage Flag to skip remaining subplots for the current page and go to the next page.
#' @param data Override earlier specified data (in pppicture or ppsubplot).
#' @param mapping Override earlier specified mapping (in pppicture or ppsubplot).
#' @return The updated PSTricks object, with respect to the attributes
#' * x0 - The position of the x axis.
#' * y0 - The position of the y axis.
#' * dx - The space allocated for the subplot in the x direction.
#' * dy - The space allocated for the subplot in the y direction.
#' * hx - The length of the x axis.
#' * hy - The length of the y axis.
#' * nx - Saved nx for subsequent subplots.
#' * ny - Saved ny for subsequent subplots.
#' * isub - Saved n for subsequent subplots.
#' * pxad - Flag to indicate that primary x axis has been drawn.
#' * pyad - Flag to indicate that primary y axis has been drawn.
#' * sxad - Flag to indicate that secondary x axis has been drawn.
#' * syad - Flag to indicate that secondary y axis has been drawn.
#' @export
#' @examples
#' pppicture(PSTricks(),data=mtcars) %>%
#'     ppsubplot(2,3,data=mtcars,mapping=aes(x=wt,y=mpg)) %>%
#'     geom_dots() %>%
#'     ppsubplot() %>%
#'     geom_dots(aes(x=wt,y=cyl))

ppsubplot <- function(p, nx=NULL, ny=NULL, n=NULL, nxaxes=1, nyaxes=1, ntitle=NULL,
                      width=1, height=1,
                      newpage=FALSE,
                      data=NULL, mapping=NULL)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    if (!is.null(p$geoms)) p <- ppgeoms(p)

    if (xor(is.null(nx),is.null(ny))) stop("both `nx` and `ny` should be specified")

    if (!is.null(nx)) p$isub <- 0

    nx     <- sifelse(is.null(nx),sifelse(is.null(p$nx),1,p$nx),nx)
    ny     <- sifelse(is.null(ny),sifelse(is.null(p$ny),1,p$ny),ny)
    n      <- sifelse(is.null(n),sifelse(is.null(p$isub),1,p$isub+1),n)
    ntitle <- sifelse(is.null(ntitle),sifelse(is.null(p$ntitle),1,p$ntitle),ntitle)

    if ((n > nx*ny) || (newpage && (n>1))) {
        p <- ppnewpage(p)
        n <- 1
    }

    p$nx <- nx
    p$ny <- ny
    p$isub <- n

    x <- p$x
    y <- p$y
    mrg <- p$margin/2

    n <- n - 1
    iy <- n %/% nx
    ix <- n - iy*nx

    dxa <- 0
    dya <- 0

    dx <- (x-(nx-1)*mrg) / nx
    dy <- (y-(ny-1)*mrg) / ny

    x0 <- ix*dx + ix*mrg
    y0 <- (ny-iy-1)*dy + (ny-iy-1)*mrg

    dx <- dx * width + mrg*(width-1)
    dy <- dy * height + mrg*(height-1)

    hx <- dx - p$margin*(1+nyaxes*1.5)
    hy <- dy - p$margin*(1+nxaxes*1.5+ntitle*0.5)

    p$dx <- dx
    p$dy <- dy
    p$x0 <- x0 + dxa
    p$y0 <- y0 + dya
    p$hx <- hx
    p$hy <- hy

    p$pxad <- FALSE
    p$pyad <- FALSE
    p$sxad <- FALSE
    p$syad <- FALSE

    if (!is.null(data)) {
        p$data <- data
        datnam <- deparse(substitute(data))
    }
    if (!is.null(mapping)) {
        if (!isa(mapping,"mapping")) stop("mapping argument is not a valid aes()")
        p$mapping <- mapping
    }
    p
}

#' Set Parameters of Subplot Manually
#' @param p The PSTricks object.
#' @param x0 The reference position of the x axis.
#' @param y0 The reference position of the y axis.
#' @param hx The length of the x axis.
#' @param hy The length of the y axis.
#' @param ntitle Number of lines to reserve for the title.
#' @return The updated PSTricks object.
#' @seealso [adjx0y0()] to get axis positions.
#' @export
#' @examples
#' pppicture(PSTricks(),20,28,par="showgrid=true") %>% ppmansubplot(2,2,8,6) %>%
#'     ppaxis('x',c(0,1)) %>% ppaxis('y',c(0,1)) %>% pptitle("title")
#' # note that (x0,y0) is the reference position, not where the axes start

ppmansubplot <- function(p, x0, y0, hx, hy, ntitle=1)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    if (!is.null(p$geoms)) p <- ppgeoms(p)

    p$x0 <- x0
    p$y0 <- y0
    p$hx <- hx
    p$hy <- hy
    p$dx <- hx + p$margin*2.5
    p$dy <- hy + p$margin*(2.5 + ntitle*0.5)

    p$pxad <- FALSE
    p$pyad <- FALSE
    p$sxad <- FALSE
    p$syad <- FALSE

    p
}

#' Calculate x for pppicture given y to get hy = aspect*hx
#'
#' @param y Desired space in y direction.
#' @param aspect Desired aspect ratio of axes.
#' @param nx Number of plots in the x direction (if NULL, increment n automatically).
#' @param ny Number of plots in the y direction.
#' @param nxaxes Number of x axes to make space for.
#' @param nyaxes Number of y axes to make space for.
#' @param ntitle Number of title lines to make space for.
#' @param width Number of subplots to occupy in the x direction.
#' @param height Number of subplots to occupy in the y direction.
#' @param margin Margin.
#' @return The x value.
#' @export
#' @examples
#' pppicture(PSTricks(),xaspect(12),12,par="showgrid=true") %>%
#'     geom_dots(aes(x=wt,y=mpg),mtcars) %>%
#'     xticks(extlabs=TRUE) %>% yticks(extlabs=TRUE) %>%
#'     pptitle("\\Large mtcars")

xaspect <- function(y, aspect=1, nx=1, ny=1, nxaxes=1, nyaxes=1, ntitle=1,
                    width=1, height=1, margin=1)
{
    mrg <- margin/2

    (nx-1)*mrg + (nx/(width*aspect)) * ((y-(ny-1)*mrg) / ny * height + mrg*(height-1) - margin*(1+nxaxes*1.5+ntitle*0.5)) -
                 (nx/width) * (mrg*(width-1) - margin*(1+nyaxes*1.5))
}

#' Calculate y for pppicture given x to get hy = aspect*hx
#'
#' @param x Desired space in x direction.
#' @param aspect Desired aspect ratio of axes.
#' @param nx Number of plots in the x direction (if NULL, increment n automatically).
#' @param ny Number of plots in the y direction.
#' @param nxaxes Number of x axes to make space for.
#' @param nyaxes Number of y axes to make space for.
#' @param ntitle Number of title lines to make space for.
#' @param width Number of subplots to occupy in the x direction.
#' @param height Number of subplots to occupy in the y direction.
#' @param margin Margin.
#' @return The y value.
#' @export
#' @examples
#' pppicture(PSTricks(),12,yaspect(12),par="showgrid=true") %>%
#'     geom_dots(aes(x=wt,y=mpg),mtcars) %>%
#'     xticks(extlabs=TRUE) %>% yticks(extlabs=TRUE) %>%
#'     pptitle("\\Large mtcars")

yaspect <- function(x, aspect=1, nx=1, ny=1, nxaxes=1, nyaxes=1, ntitle=1,
                    width=1, height=1, margin=1)
{
    mrg <- margin/2

    (ny-1)*mrg + (ny/height)*(aspect * ((x-(nx-1)*mrg) / nx * width + mrg*(width-1) - margin*(1+nyaxes*1.5)) -
                              (mrg*(height-1) - margin*(1+nxaxes*1.5+ntitle*0.5)))
}

#' Calculate x,y for pppicture given x,y (in p) to get hy = aspect*hx
#'
#' @param p The PSTricks object.
#' @param aspect Desired aspect ratio of axes.
#' @param nx Number of plots in the x direction (if NULL, increment n automatically).
#' @param ny Number of plots in the y direction.
#' @param nxaxes Number of x axes to make space for.
#' @param nyaxes Number of y axes to make space for.
#' @param ntitle Number of title lines to make space for.
#' @param width Number of subplots to occupy in the x direction.
#' @param height Number of subplots to occupy in the y direction.
#' @param margin Margin.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' PSTricks() %>% xyaspect(ntitle=0) %>% pppicture(par="showgrid=true") %>%
#'     geom_dots(aes(x=wt,y=mpg),mtcars)

xyaspect <- function(p, aspect=1, nx=1, ny=1, nxaxes=1, nyaxes=1, ntitle=1,
                    width=1, height=1, margin=1)
{
    x <- xaspect(p$y, aspect, nx, ny, nxaxes, nyaxes, ntitle, width, height, margin)
    y <- yaspect(p$x, aspect, nx, ny, nxaxes, nyaxes, ntitle, width, height, margin)

    p$margin <- margin

    p$nx <- nx
    p$ny <- ny
    p$ntitle <- ntitle

    p$xasp <- sifelse(x>p$x, NULL, x)
    p$yasp <- sifelse(y>p$y, NULL, y)

    p
}
