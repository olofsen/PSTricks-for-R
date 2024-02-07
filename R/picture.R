#' Open a Picture and Prepare for using PSTricks Functions
#' @details
#' `pppicture` is not called `pspicture` because of the large difference in functionality.
#' It is not needed for using PSTricks package per se (as in LaTeX itself).
#' Most examples use `pppicture()`.
#' @param p The PSTricks object.
#' @param x,y Coordinates of upper right corner (and optionally lower left corner).
#' @param data Data to use with geoms.
#' @param mapping Mapping to use with geoms.
#' @param par Parameters for the underlying `pspicture` macro (see Voss' latest documentation).
#' @param star Flag to indicate that objects should be clipped with respect to the boundaries.
#' @return The updated PSTricks object with initial default values for the attributes
#' * datnam - Name of the data for reference.
#' * data - Data for geoms.
#' * mapping - Mapping for geoms.
#' * geoms - List of called geoms.
#' * xlim,ylim - Range of `x` and `y` data.
#' * xlab,ylab - Labels for the x and y axes.
#' * xlabsep,ylabsep - Distance between tickmark and axes labels.
#' * xa,xb,ya,yb - Scaling conversion parameters.
#' * xticks - See below.
#' * yticks - See below.
#' * logx,logy - Flags to indicate logarithmic x and/or y axes.
#' * secondx,secondy - Flags to indicate secondary x and/or y axes.
#' * pxad,pyad,sxad,syad - Flags to indicate which axes have been drawn.
#' * margin - Parameter that determines the layout of a graph.
#' * mrgaxes - A factor for the margins between the axes.
#' * polar - Flag to indicate whether coordinates should be interpreted as polar.
#' * degrees - The number of units in a circle.
#' * linewidth - The default line width in cm.
#' * picpar - Parameters saved for a possible subsequent `pspicture` with `ppnewpage()`.
#' * psttoeps - Flag to indicate that the PSTtoEPS feature should be used with geoms.
#'
#' `xticks` and `yticks` are lists with the items
#' * nticks -Number of tickmarks; if nticks=0, pretty tickmarks will be determined automatically.
#' * mticks - Number of minor tickmarks.
#' * nolabels - Flag to indicate that no labels should be printed.
#' * extlabs - Flag to indicate that labels at axis extrema should be printed.
#' * labels - List of labels instead of numbers to print at the tickmarks.
#' * ticklength - The length of the ticks.
#' * ticklengthi - The inward length of the ticks (default same as outward).
#' * rotation - The rotation for the labels at the tickmarks.
#' @seealso
#' See [tvput()] for a rare example where `pppicture()` is not used.
#' And see [pspicture()] for the lower level function.
#' @export

pppicture <- function(p, x=NULL, y=NULL, data=NULL, mapping=NULL, par=NULL, star=FALSE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    if (!is.null(mapping)) {
        if (!isa(mapping,"mapping")) stop("mapping argument is not a valid aes()")
    }

    if (!p$docOpened) p <- ppopendoc(p)
    if (p$picOpened) p <- endpppicture(p,"\\\\")


    if (length(x)>1) {
        x0 <- x[1]
        x <- x[2]
    } else {
        x0 <- NULL
    }
    if (length(y)>1) {
        y0 <- y[1]
        y <- y[2]
    } else {
        y0 <- NULL
    }

## possible xyaspect calculations

    if (!is.null(p$xasp)) x <- p$xasp
    if (!is.null(p$yasp)) y <- p$yasp

    if (!is.null(x)) {
        dx <- (p$x-(x-sifelse(is.null(x0),0,x0)))/2 # for \hoffset below
        p$x <- x
    } else {
        dx <- 0
    }
    if (!is.null(y)) {
        dy <- (p$y-(y-sifelse(is.null(y0),0,y0)))/2 # for \voffset below
        p$y <- y
    } else {
        dy <- 0
    }

    if (!is.null(par)) par <- paste0("[",par,"]")

    attrs <- list(datnam=deparse(substitute(data)),
                  data=data,
                  mapping=mapping,
                  geoms=NULL,
                  xlim=NULL,
                  ylim=NULL,
                  xlab=NULL,
                  ylab=NULL,
                  xlabsep=0.7,
                  ylabsep=1,
                  xa=1, xb=0, ya=1, yb=0,
                  xticks=list(nticks=0, mticks=0, nolabels=FALSE, extlabs=FALSE, labels=NULL, rotation=0,
                              ticklength=0.2, ticklengthi=NULL),
                  yticks=list(nticks=0, mticks=0, nolabels=FALSE, extlabs=FALSE, labels=NULL, rotation=0,
                              ticklength=0.2, ticklengthi=NULL),
                  logx=FALSE,
                  logy=FALSE,
                  secondx=FALSE,
                  secondy=FALSE,
                  pxad=FALSE,
                  pyad=FALSE,
                  sxad=FALSE,
                  syad=FALSE,
                  margin=sifelse(is.null(p$margin),1,p$margin), # may have been assigned already
                  mrgaxes=sifelse(is.null(p$mrgaxes),1,p$mrgaxes),
                  polar=FALSE,
                  degrees=360,
                  linewidth=0.8*2.54/72, # = 0.02822222, .8 pt in cm
                  picpar=list(x0=x0,y0=y0,par=par,star=star),
                  psttoeps=FALSE)

    p <- structure(merge(attrs,p),class=class(p)) # merge.list from utils.R

    if (p$center) {
        p <- p %>%
            ppappend(paste0("\\setlength{\\hoffset}{",dx,"cm}")) %>%
            ppappend(paste0("\\setlength{\\voffset}{",dy,"cm}"))
        p$center <- FALSE
    }

    p <- p %>%
        ppappend(paste0("\\pspicture",sifelse(star,'*',''),par,ppcoords(,x0,y0),ppcoords(,p$x,p$y))) %>%
        ppsubplot(,,1)
    p$picOpened <- TRUE
    p
}

#' Open a Default Picture
#' @details Used by geoms if no picture has been opened.
#' @param p The PSTricks object.
#' @return The updated PSTricks object.
#' @export

ppdefpicture <- function(p)
{
    if (!p$picOpened) p <- pppicture(p,16,9) # open if needed

    p
}

#' Close the Current Picture and Open a New One
#' @details Lower level option values will be reset, but higher level options will not.
#' @param p The PSTricks object.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(engine="latex"),16,9, data=mtcars, par="showgrid=true") %>%
#'     geom_dots(aes(x=wt,y=mpg)) %>%
#'     pptitle("\\Large picture 1") %>%
#'     ppnewpage() %>%
#'     geom_dots(aes(x=wt,y=cyl)) %>%
#'     pptitle("\\Large picture 2")
#' # Engine pdflatex gives one page...

ppnewpage <- function(p)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    if (!p$docOpened) stop("document should be open")
    if (p$picOpened) p <- endpppicture(p,"\\\\")

    p <- p %>%
        ppappend("\\newpage") %>%
        ppappend(paste0("\\pspicture",sifelse(p$picpar$star,'*',''),
                        p$picpar$par,ppcoords(,p$picpar$x0,p$picpar$y0),ppcoords(,p$x,p$y))) %>%
        ppsubplot(,,1)
    p$picOpened <- TRUE
    p
}

#' Close the Picture
#' @param p The PSTricks object.
#' @param ending String to end the pppicture environment with.
#' @return The updated PSTricks object.
#' @export

endpppicture <- function(p, ending="")
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    if (!p$picOpened) return(p)
    if (!is.null(p$geoms)) p <- ppgeoms(p)

    p <- ppappend(p,paste0("\\endpspicture",ending))
    p$picOpened <- FALSE
    p
}
