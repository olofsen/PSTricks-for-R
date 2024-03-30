#' Construct Aesthetic Mappings
#' @details Note: `aes()` does not evaluate right hand sides of mappings.
#' @param ... Comma separated mappings such as in the example below.
#' @return A structure containing the mapping.
#' @seealso [geom_set()] for an example.
#' @export
#' @examples
#' aes("x=time")

aes <- function(...)
{
    structure(match.call(),
              class="mapping")
}

#' Set PSTricks Parameter(s) during Geom Processing
#' @param p The PSTricks object.
#' @param par PSTricks (comma separated) parameter(s).
#' @return The updated PSTricks object.
#' @seealso [psset()] for the base version.
#' @export
#' @examples
#' mtcars<-cbind(mtcars,stuff=row.names(mtcars));
#' PSTricks() %>%
#'     pppicture(16,26) %>%
#' # the following three commands affect the axes
#'     psset("arrows=c-c") %>%
#'     pplinewidth(.3) %>%
#'     everypsbox("\\large") %>%
#' # the following three commands affect the frameboxes
#'     geom_set("framearc=.3,fillstyle=solid,fillcolor=darkgray") %>%
#'     geom_linewidth(.1) %>%
#'     geom_everypsbox("\\green") %>%
#'     geom_framebox(aes(x=wt,y=mpg),mtcars[mtcars$cyl==4,]) %>%
#'     geom_linewidth(.3) %>%
#'     geom_everypsbox("\\cyan") %>%
#'     geom_framebox(aes(x=wt,y=mpg),mtcars[mtcars$cyl==6,]) %>%
#'     geom_linewidth(.5) %>%
#'     geom_everypsbox("\\red") %>%
#'     geom_framebox(aes(x=wt,y=mpg),mtcars[mtcars$cyl==8,]) %>%
#'     lims(c(1,6),c(10,35)) %>%
#'     labs("Weight (lb/1000)","Fuel efficiency (miles/gallon)") %>%
#'     pplegend("4 cylinders",par="linecolor=green",dx=-3) %>%
#'     pplegend("6 cylinders",par="linecolor=cyan",dx=-3,dy=-.5) %>%
#'     pplegend("8 cylinders",par="linecolor=red",dx=-3,dy=-1)

geom_set <- function(p, par)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, par=par),
                   class=c("geom",name))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Set PSTricks' `linewidth` Parameter during Geom Processing
#' @param p The PSTricks object.
#' @param linewidth The linewidth to use (default the PSTricks default (0.8 pt)).
#' @return The updated PSTricks object.
#' @seealso [pplinewidth()] for the base version and [geom_set()] for an example.
#' @export

geom_linewidth <- function(p, linewidth=0.8*2.54/72)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, linewidth=linewidth),
                   class=c("geom","geom_set"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Set `everypsbox` during Geom Processing
#' @param p The PSTricks object.
#' @param par Stuff to apply to a `psbox`.
#' @return The updated PSTricks object.
#' @seealso [everypsbox()] for the base version and [geom_set()] for an example.
#' @export

geom_everypsbox <- function(p, par=NULL)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, par=par),
                   class=c("geom","geom_set"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Connect Observations using Lines
#' @param p The PSTricks object.
#' @param mapping Aesthetic mapping from column names to `x` and `y`.
#' @param data Data frame with coordinates of the observations.
#' @param par PSTricks parameter string.
#' @param dodge Horizontal offset.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [psline()] for the base version.
#' @export
#' @examples
#' geom_line(PSTricks(),aes(x=xdata,y=ydata),data.frame(xdata=c(4,0,2),ydata=c(2,1,0)),
#'     "linewidth=2pt,linearc=.25,arrows=->")
#' # Note that the names in the data frame determine the axis label names by default
#' # and that a default `pppicture()` is called automatically

geom_line <- function(p, mapping=NULL, data=NULL, par=NULL, dodge=0, star=FALSE)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, mapping=mapping, data=data, par=par, dodge=dodge, star=star),
                   class=c("geom","geom_xy"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Draw Polygons
#' @param p The PSTricks object.
#' @param mapping Aesthetic mapping from column names to `x` and `y`.
#' @param data Data frame with coordinates of the observations.
#' @param par PSTricks parameter string.
#' @param dodge Horizontal offset.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [pspolygon()] for the base version.
#' @export
#' @examples
#' PSTricks() %>%
#'     geom_polygon(data=data.frame(x=c(0,0,1),y=c(0,2,2)),par="linewidth=1.5pt") %>%
#'     geom_polygon(data=data.frame(x=c(1,1,4,4),y=c(0,2,0,2)),par="linearc=.2",star=TRUE)
#' # Note that the first coordinate (0,0) for the first polygon has to be given explicitly

geom_polygon <- function(p, mapping=NULL, data=NULL, par=NULL, dodge=0, star=FALSE)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, mapping=mapping, data=data, par=par, dodge=dodge, star=star),
                   class=c("geom","geom_xy"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Draw Frames
#' @param p The PSTricks object.
#' @param mapping Aesthetic mapping from column names to `x` and `y`.
#' @param data Data frame with coordinates of the observations.
#' @param par PSTricks parameter string.
#' @param dodge Horizontal offset.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [psframe()] for the base version.
#' @export
#' @examples
#' geom_frame(PSTricks(),
#'     data=data.frame(x0=c(0,1),x1=c(4,.5),y0=c(0,.5),y1=c(2,1.5),
#'         par=c("linewidth=2pt,framearc=.3,fillstyle=solid,fillcolor=lightgray",
#'            "linecolor=white"),
#'         star=c(FALSE,TRUE)))

geom_frame <- function(p, mapping=NULL, data=NULL, par=NULL, dodge=0, star=FALSE)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, mapping=mapping, data=data, par=par, dodge=dodge, star=star),
                   class=c("geom",name))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Add Frameboxes
#' @param p The PSTricks object.
#' @param mapping Aesthetic mapping from column names to `x` and `y`.
#' @param data Data frame with coordinates of the observations.
#' @param par PSTricks parameter string.
#' @param refpoint The reference point for the stuff.
#' @param rotation Rotation to apply to the stuff.
#' @param dodge Horizontal offset.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [psframebox()] and [rput()] for the base versions and [geom_set()] for an example.
#' @export

geom_framebox <- function(p, mapping=NULL, data=NULL, par=NULL, refpoint=NULL, rotation=NULL, dodge=0, star=FALSE)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, mapping=mapping, data=data, par=par, refpoint=refpoint, rotation=rotation,
                        dodge=dodge, star=star),
                   class=c("geom","geom_xy"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Connect Observations using Smooth Lines
#' @param p The PSTricks object.
#' @param mapping Aesthetic mapping from column names to `x` and `y`.
#' @param data Data frame with coordinates of the observations.
#' @param par PSTricks parameter string.
#' @param dodge Horizontal offset.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [pscurve()] for the base version.
#' @export
#' @examples
#' PSTricks() %>%
#'     newrgbcolor("verylightgray",.9,.9,.9) %>%
#'     ppsetmargins(mrgaxes=0) %>%
#'     geom_grid("linestyle=dotted,linecolor=gray",
#'         background="verylightgray") %>%
#'     geom_curve(data=data.frame(x=c(0,.7,3.3,4,.4),y=c(1.3,1.8,.5,1.6,.4)),
#'         par="showpoints=true") %>%
#'     geom_legend("top right","showpoints=true") %>%
#'     xlim(-1,5) %>% ylim(0,2)
#' # Note that autoscaling which uses the data only does not work optimally

geom_curve <- function(p, mapping=NULL, data=NULL, par=NULL, dodge=0, star=FALSE)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, mapping=mapping, data=data, par=par, dodge=dodge, star=star),
                   class=c("geom","geom_xy"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Connect Observations using Smooth Lines
#' @param p The PSTricks object.
#' @param mapping Aesthetic mapping from column names to `x` and `y`.
#' @param data Data frame with coordinates of the observations.
#' @param par PSTricks parameter string.
#' @param dodge Horizontal offset.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [psecurve()] for the base version.
#' @export
#' @examples
#' PSTricks() %>%
#'     pppicture(16,9,star=TRUE) %>%
#'     geom_ecurve(data=data.frame(x=c(.125,.25,.5,1,2,4,8),
#'                                 y=c(8,4,2,1,.5,.25,.125)),
#'                 par="showpoints=true") %>%
#'     xlim(0,4) %>% ylim(0,4) %>%
#'     geom_grid()

geom_ecurve <- function(p, mapping=NULL, data=NULL, par=NULL, dodge=0, star=FALSE)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, mapping=mapping, data=data, par=par, dodge=dodge, star=star),
                   class=c("geom","geom_xy"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Connect Observations using Smooth Lines
#' @param p The PSTricks object.
#' @param mapping Aesthetic mapping from column names to `x` and `y`.
#' @param data Data frame with coordinates of the observations.
#' @param par PSTricks parameter string.
#' @param dodge Horizontal offset.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [psccurve()] for the base version.
#' @export
#' @examples
#' geom_ccurve(PSTricks(),
#'     data=data.frame(x=c(.5,3.5,3.5,.5),y=c(0,1,0,1)),
#'         par="showpoints=true") %>%
#'     xlim(0,4) %>% ylim(-0.5,1.5) %>%
#'     geom_grid()

geom_ccurve <- function(p, mapping=NULL, data=NULL, par=NULL, dodge=0, star=FALSE)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, mapping=mapping, data=data, par=par, dodge=dodge, star=star),
                   class=c("geom","geom_xy"))
    p$geoms <- append(p$geoms,list(g))
    p
}

## not ggplot2's geom_point, to map geom_* to ps_* consistently

#' Plot Dots
#' @param p The PSTricks object.
#' @param mapping Aesthetic mapping from column names to `x` and `y`.
#' @param data Data frame with coordinates of the observations.
#' @param par PSTricks parameter string.
#' @param dodge Horizontal offset.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [psdots()] for the base version and [geom_abline()] for another example.
#' @export
#' @examples
#' geom_dots(PSTricks(),data=data.frame(x=c(0,1,2),y=c(1,1,1),
#'     par=paste0("dotstyle=",c('*','o','Bo'))))

geom_dots <- function(p, mapping=NULL, data=NULL, par=NULL, dodge=0, star=FALSE)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, mapping=mapping, data=data, par=par, dodge=dodge, star=star),
                   class=c("geom","geom_xy"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Plot Circles
#' @param p The PSTricks object.
#' @param mapping Aesthetic mapping from column names to `x` and `y` (and optionally `radius`).
#' @param data Data frame with properties of the circles.
#' @param radius Radius of the circles.
#' @param par PSTricks parameter string.
#' @param dodge Horizontal offset.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [pscircle()] for the base version.
#' @export
#' @examples
#' geom_circle(PSTricks(),data=data.frame(x=c(0,1,2),y=c(1,1,1)),radius=0.2)

geom_circle <- function(p, mapping=NULL, data=NULL, radius=NULL, par=NULL, dodge=0, star=FALSE)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, mapping=mapping, data=data, radius=radius, par=par, dodge=dodge, star=star),
                   class=c("geom","geom_xy"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Add Text Items
#' @param p The PSTricks object.
#' @param mapping Aesthetic mapping from column names to `x` and `y`.
#' @param data Data frame with coordinates of the observations.
#' @param refpoint The reference point for the stuff.
#' @param rotation Rotation to apply to the stuff.
#' @param dodge Horizontal offset.
#' @param star Flag to indicate starred version (but see `geom_framebox()`).
#' @return The updated PSTricks object.
#' @seealso [rput()] for the base version.
#' @export
#' @examples
#' geom_rput(PSTricks(),
#'     aes(x=wt,y=mpg,stuff=stuff),
#'     cbind(mtcars,stuff=row.names(mtcars)),
#'     rotation=45,
#'     star=TRUE)

geom_rput <- function(p, mapping=NULL, data=NULL, refpoint=NULL, rotation=NULL, dodge=0, star=FALSE)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, mapping=mapping, data=data, refpoint=refpoint, rotation=rotation, dodge=dodge, star=star),
                   class=c("geom","geom_xy"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Add Text Items
#' @param p The PSTricks object.
#' @param mapping Aesthetic mapping from column names to `x` and `y`.
#' @param data Data frame with coordinates of the observations.
#' @param refangle The reference angle.
#' @param rotation Rotation to apply to the stuff.
#' @param labelsep Distance between coordinates and the stuff.
#' @param dodge Horizontal offset.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [uput()] for the base version.
#' @export
#' @examples
#' geom_uput(PSTricks(),
#'     aes(x=wt,y=mpg,stuff=stuff),
#'     cbind(mtcars,stuff=row.names(mtcars)),
#'     refangle=0,
#'     rotation=45,
#'     star=TRUE)

geom_uput <- function(p, mapping=NULL, data=NULL, refangle=NULL, rotation=NULL, labelsep=NULL, dodge=0, star=FALSE)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, mapping=mapping, data=data, refangle=refangle, rotation=rotation, labelsep=labelsep,
                        dodge=dodge, star=star),
                   class=c("geom","geom_xy"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Draw Straight Line
#' @param p The PSTricks object.
#' @param slope The slope of the line, or an `lm` object.
#' @param intercept The intercept of the line.
#' @param par PSTricks parameter string.
#' @return The updated PSTricks object.
#' @importFrom stats coef model.frame
#' @export
#' @examples
#' PSTricks() %>%
#'     pppicture(16,9) %>%
#'     ppsetlogxy() %>%
#'     geom_dots(aes(x=hp,y=mpg),mtcars,par="dotstyle=Bo") %>%
#'     geom_abline(lm(log10(mpg)~log10(hp),data=mtcars),par="linecolor=red") %>%
#'     geom_hline(20,par="linecolor=green") %>%
#'     geom_vline(100,par="linecolor=blue")
#' # Note that log10 needs to be used for lm with log axes

geom_abline <- function(p, slope=1, intercept=0, par=NULL)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    if (is(slope,"lm")) {
        fm <- slope
        intercept <- coef(fm)[[1]]
        slope <- coef(fm)[[2]]
        nms <- names(model.frame(fm))
        xname <- nms[2]
        yname <- nms[1]
    } else {
        xname <- 'x'
        yname <- 'y'
    }

    p <- ppdefpicture(p)

    g <- structure(list(name=name, slope=slope, intercept=intercept,
                        x=list(name=xname), y=list(name=yname), par=par),
                   class=c("geom","geom_line"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Draw Horizontal Line
#' @param p The PSTricks object.
#' @param yintercept The y-intercept of the line.
#' @param par PSTricks parameter string.
#' @return The updated PSTricks object.
#' @seealso [geom_abline()] for an example.
#' @export

geom_hline <- function(p, yintercept=0, par=NULL)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, yintercept=yintercept, par=par),
                   class=c("geom","geom_line"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Draw Vertical Line
#' @param p The PSTricks object.
#' @param xintercept The x-intercept of the line.
#' @param par PSTricks parameter string.
#' @return The updated PSTricks object.
#' @seealso [geom_abline()] for an example.
#' @export

geom_vline <- function(p, xintercept=0, par=NULL)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, xintercept=xintercept, par=par),
                   class=c("geom","geom_line"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Vertical Errorbars
#' @param p The PSTricks object.
#' @param mapping Aesthetic mapping from column names to `x`, `y`, `ymin`, and `ymax`.
#' @param data Data frame with values for the error bars.
#' @param par PSTricks parameters.
#' @param width Horizontal width of the error bars.
#' @param dodge Horizontal offset.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),16,9,data=data.frame(x=c(1,2,3,4),y=c(2,4,6,8),
#'                                           ymin=c(1,2,3,4),ymax=c(3,6,9,12))) %>%
#'     geom_set("linecolor=blue") %>%
#'     geom_line(par="showpoints=true",dodge=-0.125) %>%
#'     geom_errorbar(dodge=-0.125) %>%
#'     geom_set("linecolor=green") %>%
#'     geom_line(par="showpoints=true",dodge=0.125) %>%
#'     geom_errorbar(aes(ymin=NA),dodge=0.125)

geom_errorbar <- function(p, mapping=NULL, data=NULL, par=NULL, width=0.1, dodge=0)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    if (!p$picOpened) p <- pppicture(p,p$x*0.7,p$y*0.3) # open default picture if needed

    g <- structure(list(name=name, mapping=mapping, data=data, par=par, width=width, dodge=dodge),
                   class=c("geom",name))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Plot a Histogram
#' @details
#' Issue: The default mapping containing "breaks" and "counts" leads to a NOTE when running "R CMD check".
#' @param p The PSTricks object.
#' @param mapping Either `aes(x=breaks,y=counts)` or `aes(x=breaks,y=density)`.
#' @param data Output of R's `hist(..., plot=FALSE)` function.
#' @param par PSTricks parameters.
#' @param star Flag to use `star` version of `psframe`.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' geom_hist(PSTricks(),data=hist(mtcars$mpg,plot=FALSE),
#'     par="fillcolor=cyan,fillstyle=solid")

geom_hist <- function(p, mapping=aes(x=breaks,y=counts), data=NULL, par="fillcolor=lightgray,fillstyle=solid", star=FALSE)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    if (!isa(data,"histogram")) stop("data is not a histogram object")

    data <- data.frame(breaks=data$breaks,
                       counts=c(data$counts,data$counts[length(data$counts)]),
                       density=c(data$density,data$density[length(data$density)]))

    p <- ppdefpicture(p)

    g <- structure(list(name=name, mapping=mapping, data=data, par=par, dodge=0, star=star),
                   class=c("geom","geom_xy"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Add Legend to Plot
#' @param p The PSTricks object.
#' @param s The legend text.
#' @param par PSTricks parameter string.
#' @param position Position for the legend (may be NULL).
#' @param dx,dy x and y offsets w.r.t. default position.
#' @param w Width of the `psline` that belongs to the legend text.
#' @param labelsep The distance between the line and the label.
#' @return The updated PSTricks object.
#' @seealso [pplegend()] for the base version and [geom_curve()] for an example.
#' @export

geom_legend <- function(p, s, par=NULL, position='tr',
                        dx=0, dy=0, w=1, labelsep="10pt")
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, s=s, par=par, position=position,
                        dx=dx, dy=dy, w=w, labelsep=labelsep),
                   class=c("geom","geom_set"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Draw Grid Lines
#' @param p The PSTricks object.
#' @param par PSTricks parameters.
#' @param background The background color.
#' @return The updated PSTricks object.
#' @seealso [ppgrid()] for the base version and [geom_curve()] for an example.
#' @export
#' @examples
#' geom_grid(PSTricks())

geom_grid <- function(p, par="linestyle=dotted", background=NULL)
{
    name <- cvtname(match.call()[[1]])
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- ppdefpicture(p)

    g <- structure(list(name=name, par=par, background=background),
                   class=c("geom","geom_set"))
    p$geoms <- append(p$geoms,list(g))
    p
}

#' Set x Axis Limits
#' @param p The PSTricks object.
#' @param xl,xu Lower and upper axis limits.
#' @return The updated PSTricks object.
#' @seealso See [geom_curve()] for an example.
#' @export

xlim <- function(p, xl=NULL, xu=NULL)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$xlim <- c(xl,xu)
    p
}

#' Set y Axis Limits
#' @param p The PSTricks object.
#' @param yl,yu Lower and upper axis limits.
#' @return The updated PSTricks object.
#' @seealso See [geom_curve()] for an example.
#' @export

ylim <- function(p, yl=NULL, yu=NULL)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$ylim <- c(yl,yu)
    p
}

#' Set x and y Axes Limits
#' @param p The PSTricks object.
#' @param x,y x and y lower and upper axis limits (two-element lists or NULL for automatic).
#' @return The updated PSTricks object.
#' @seealso [geom_set()] for an example.
#' @export

lims <- function(p, x=NULL, y=NULL)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    
    if (!is.null(x)) if (length(x)!=2) stop("x limits needs to be a vector of length 2")
    if (!is.null(y)) if (length(y)!=2) stop("y limits needs to be a vector of length 2")

    p %>%
        xlim(x[1],x[2]) %>%
        ylim(y[1],y[2])
}

#' Set x Axis Label
#' @param p The PSTricks object.
#' @param lab x axis label.
#' @return The updated PSTricks object.
#' @export

xlab <- function(p, lab)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$xlab <- lab
    p
}

#' Set y Axis Label
#' @param p The PSTricks object.
#' @param lab y axis label.
#' @return The updated PSTricks object.
#' @export

ylab <- function(p, lab)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$ylab <- lab
    p
}

#' Set Axis Labels and Title
#' @param p The PSTricks object.
#' @param x,y x and y axis labels.
#' @param title The title for the plot.
#' @return The updated PSTricks object.
#' @seealso [geom_set()] for an example.
#' @export

labs <- function(p, x, y, title=NULL)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    p <- p %>%
        xlab(x) %>%
        ylab(y)

    if (!is.null(title)) p <- pptitle(p, title)

    p
}

#' Get Variable
#' @param p The PSTricks object.
#' @param g The geom object.
#' @param v The variable to get.
#' @return The data associated with `v`.
#' @noRd

getvar <- function(p, g, v)
{
    if (is.null(g$data)) data <- p$data else data <- g$data

    if (is.null(g$mapping)) {
        name <- NULL
    } else {
        if (!isa(g$mapping,"mapping")) stop(paste0("getting `",v,"`: mapping argument is not a valid aes()"))
        name <- g$mapping[[v]]
    }
    if (is.null(name)) name <- p$mapping[[v]]
    if (is.null(name)) name <- v

    data[[name]]
}

#' Add Variable to `geom` Object
#' @param p The PSTricks object.
#' @param g The geom object.
#' @param v The variable to get information about.
#' @param log Flag to indicate that data will be log-transformed.
#' @return A list with the following information.
#' * data data
#' * idx indices of valid data
#' * limits range of name item in data
#' @noRd

addvar <- function(p, g, v, log)
{
    if (is.null(g$data)) data <- p$data else data <- g$data

    if (is.null(g$mapping)) {
        name <- NULL
    } else {
        if (!isa(g$mapping,"mapping")) stop(paste0("getting `",v,"`: mapping argument is not a valid aes()"))
        name <- g$mapping[[v]]
    }
    if (is.null(name)) name <- p$mapping[[v]]
    if (is.null(name)) name <- v

    x <- data[[name]]

    if (is.null(x)) {

        g[[v]] <- NULL

    } else {

        n <- length(x)
        idx <- !is.na(x)
        nok <- length(which(idx))
        if (nok == 0) {
            stop(paste0("`",name,"` data contain NAs only"))
        } else if (nok < n) {
            warning(paste0("NAs removed from `",name,"` data"))
        }

        if (log) {
            isng <- x<0
            nisng <- length(which(isng))
            if (nisng > 0) warning(paste0("values<=0 removed from `",name,"` data"))
            idx[isng] <- FALSE
        }

        n <- length(which(idx))
        if (n == 0) stop(paste0("no `",name,"` data to plot"))

        idxl <- idx & !is.infinite(x)
        limits <- range(x[idxl])

        g[[v]] <- list(name=name, data=x, idx=idx, limits=limits)

    }

    g
}

#' Process Geoms
#' @details
#' `ppgeoms()` is called automatically when the current subplot is closed. The example
#' given below shows an instance where it is necessary to call it explicitly.
#' @param p The PSTricks object.
#' @return The updated PSTricks object.
#' @importFrom methods is
#' @export
#' @examples
#' pppicture(PSTricks(),16,9,data=mtcars) %>%
#'     geom_dots(aes(x=wt,y=mpg),par="linecolor=green") %>%
#'     ppgeoms() %>%
#'     ppsetsecondary('y') %>%
#'     geom_dots(aes(x=wt,y=cyl),par="linecolor=blue")

ppgeoms <- function(p)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    if (is.null(p$geoms)) return(p)

    xlimits <- c(NA,NA)
    ylimits <- c(NA,NA)

    ng <- length(p$geoms)

    for (i in 1:ng) {
        g <- p$geoms[[i]]
        name <- g$name

        if (is(g,"geom_set") || is(g,"geom_line")) next

        if (is(g,"geom_xy")) {
            g <- addvar(p, g, 'x', p$logx)
            if (is.null(g[['x']])) stop("data item `x` not available")
            g <- addvar(p, g, 'y', p$logy)
            if (is.null(g[['y']])) stop("data item `y` not available")
            xlimits <- range(c(xlimits,g[['x']]$limits),na.rm=TRUE)
            ylimits <- range(c(ylimits,g[['y']]$limits),na.rm=TRUE)
        }
        if (name == "geom_frame") {
            g <- addvar(p, g, 'x0', p$logx)
            if (is.null(g[['x0']])) stop("data item `x0` not available")
            g <- addvar(p, g, 'x1', p$logx)
            if (is.null(g[['x1']])) stop("data item `x1` not available")
            g <- addvar(p, g, 'y0', p$logy)
            if (is.null(g[['y0']])) stop("data item `y0` not available")
            g <- addvar(p, g, 'y1', p$logy)
            if (is.null(g[['y1']])) stop("data item `y1` not available")
            xlimits <- range(c(xlimits,g[['x0']]$limits,g[['x1']]$limits),na.rm=TRUE)
            ylimits <- range(c(ylimits,g[['y0']]$limits,g[['y1']]$limits),na.rm=TRUE)
        } else if (name == "geom_hline") {
            ylimits <- range(c(ylimits,g$yintercept),na.rm=TRUE)
        } else if (name == "geom_vline") {
            xlimits <- range(c(xlimits,g$xintercept),na.rm=TRUE)
        } else if (name == "geom_errorbar") {
            g <- addvar(p, g, 'x', p$logx)
            if (is.null(g[['x']])) stop("data item `x` not available")
            xlimits <- range(c(xlimits,g[['x']]$limits),na.rm=TRUE)
            g <- addvar(p, g, 'y', p$logy)
            g <- addvar(p, g, 'ymin', p$logy)
            g <- addvar(p, g, 'ymax', p$logy)
            if ((is.null(g[['y']])+is.null(g[['ymin']])+is.null(g[['ymax']])) > 1) stop("not enough data items available")
            ylimits <- range(c(ylimits,g[['ymin']]$limits,g[['ymax']]$limits),na.rm=TRUE)
        }

        p$geoms[[i]] <- g # update geom with additional information
    }

## possibly override limits

    xlimits <- c(sifelse(is.null(p$xlim[1])||is.na(p$xlim[1]),xlimits[1],p$xlim[1]),
                 sifelse(is.null(p$xlim[2])||is.na(p$xlim[2]),xlimits[2],p$xlim[2]))
    ylimits <- c(sifelse(is.null(p$ylim[1])||is.na(p$ylim[1]),ylimits[1],p$ylim[1]),
                 sifelse(is.null(p$ylim[2])||is.na(p$ylim[2]),ylimits[2],p$ylim[2]))

## ppaxis handles equal lower and upper limits

    if (anyNA(xlimits)) xlimits <- c(0,0)
    if (anyNA(ylimits)) ylimits <- c(0,0)

## first informative geom determines labels

    if (!is.null(p$xlab)) {
        xlabel <- p$xlab
    } else {
        for (i in 1:ng) {
            xlabel <- p$geoms[[i]][['x']]$name
            if (!is.null(xlabel)) break
        }
    }

    if (!is.null(p$ylab)) {
        ylabel <- p$ylab
    } else {
        for (i in 1:ng) {
            ylabel <- p$geoms[[i]][['y']]$name
            if (!is.null(ylabel)) break
        }
    }

## defaults if nothing is available

    if (is.null(xlabel)) xlabel <- "x"
    if (is.null(ylabel)) ylabel <- "y"

## axes can only be redrawn after a new pppicture or ppsubplot

    if (!p$secondx && !p$pxad) {
        p <- ppaxis(p, 'x', xlimits, label=xlabel, secondary=FALSE)
        p$pxad <- TRUE
    }
    if (!p$secondy && !p$pyad) {
        p <- ppaxis(p, 'y', ylimits, label=ylabel, secondary=FALSE)
        p$pyad <- TRUE
    }
    if (p$secondx && !p$sxad) {
        p <- ppaxis(p, 'x', xlimits, label=xlabel, secondary=TRUE)
        p$sxad <- TRUE
    }
    if (p$secondy && !p$syad) {
        p <- ppaxis(p, 'y', ylimits, label=ylabel, secondary=TRUE)
        p$syad <- TRUE
    }

    if (p$psttoeps) p <- startP2E(p)

    for (g in p$geoms) {
        name <- g$name
        if (is(g,"geom_xy")) {
            x <- g[['x']]
            y <- g[['y']]
            idx <- x$idx & y$idx
            if (length(which(idx)) == 0) stop("no data to plot")
            x <- cx(p,x$data[idx]) + g$dodge
            y <- cy(p,y$data[idx])
        }
        if (name == "geom_set") {
            p <- psset(p, g$par)
        } else if (name == "geom_linewidth") {
            p <- pplinewidth(p, g$linewidth)
        } else if (name == "geom_everypsbox") {
            p <- everypsbox(p, g$par)
        } else if (name == "geom_line") {
            p <- psline(p, x, y, g$par, star=g$star)
        } else if (name == "geom_polygon") {
            p <- pspolygon(p, x, y, g$par, star=g$star)
        } else if (name == "geom_frame") {
            x0 <- g[['x0']]
            x1 <- g[['x1']]
            y0 <- g[['y0']]
            y1 <- g[['y1']]
            x0 <- cx(p,x0$data) + g$dodge
            x1 <- cx(p,x1$data) + g$dodge
            y0 <- cy(p,y0$data)
            y1 <- cy(p,y1$data)
            dxy <- p$linewidth/2
            par <- getvar(p, g, "par")
            star <- getvar(p, g, "star")
            for (i in 1:length(x0)) p <- psframe(p, c(x0[i]-dxy,x1[i]+dxy), c(y0[i]-dxy,y1[i]+dxy),
                                                 par=sifelse(is.null(par),g$par,par[i]),
                                                 star=sifelse(is.null(star),g$star,star[i]))
        } else if (name == "geom_framebox") {
            stuff <- getvar(p, g, "stuff")
            if (is.null(stuff)) stop("geom_framebox: data item `stuff` not available")
            par <- getvar(p, g, "par")
            refpoint <- getvar(p, g, "refpoint")
            rotation <- getvar(p, g, "rotation")
            star <- getvar(p, g, "star")
            for (i in 1:length(x)) {
                p <- rput(p, x[i], y[i], psframebox(, stuff[i],
                                                    sifelse(is.null(par),g$par,par[i]),
                                                    sifelse(is.null(star),g$star,star[i])),
                          sifelse(is.null(refpoint),g$refpoint,refpoint[i]),
                          sifelse(is.null(rotation),g$rotation,rotation[i]))
            }
        } else if (name == "geom_curve") {
            p <- pscurve(p, x, y, g$par, star=g$star)
        } else if (name == "geom_ecurve") {
            p <- psecurve(p, x, y, g$par, star=g$star)
        } else if (name == "geom_ccurve") {
            p <- psccurve(p, x, y, g$par, star=g$star)
        } else if (name == "geom_dots") {
            par <- getvar(p, g, "par")
            star <- getvar(p, g, "star")
            if (is.null(par) && is.null(star)) {
                p <- psdots(p, x, y, g$par, star=g$star)
            } else {
                for (i in 1:length(x)) p <- psdot(p, x[i], y[i],
                                                  par=sifelse(is.null(par),g$par,par[i]),
                                                  star=sifelse(is.null(star),g$star,star[i]))
            }
        } else if (name == "geom_circle") {
            radius <- getvar(p, g, "radius")
            par <- getvar(p, g, "par")
            star <- getvar(p, g, "star")
            for (i in 1:length(x)) p <- pscircle(p, x[i], y[i],
                                                 radius=sifelse(is.null(radius),g$radius,radius[i]),
                                                 par=sifelse(is.null(par),g$par,par[i]),
                                                 star=sifelse(is.null(star),g$star,star[i]))
        } else if (name == "geom_rput") {
            stuff <- getvar(p, g, "stuff")
            if (is.null(stuff)) stop("geom_rput: data item `stuff` not available")
            refpoint <- getvar(p, g, "refpoint")
            rotation <- getvar(p, g, "rotation")
            star <- getvar(p, g, "star")
            for (i in 1:length(x)) {
                p <- rput(p, x[i], y[i], stuff[i],
                          sifelse(is.null(refpoint),g$refpoint,refpoint[i]),
                          sifelse(is.null(rotation),g$rotation,rotation[i]),
                          sifelse(is.null(star),g$star,star[i]))
            }
        } else if (name == "geom_uput") {
            stuff <- getvar(p, g, "stuff")
            if (is.null(stuff)) stop("geom_uput: data item `stuff` not available")
            refangle <- getvar(p, g, "refangle")
            rotation <- getvar(p, g, "rotation")
            labelsep <- getvar(p, g, "labelsep")
            star <- getvar(p, g, "star")
            for (i in 1:length(x)) {
                p <- uput(p, x[i], y[i], stuff[i],
                          sifelse(is.null(refangle),g$refangle,refangle[i]),
                          sifelse(is.null(rotation),g$rotation,rotation[i]),
                          sifelse(is.null(labelsep),g$labelsep,labelsep[i]),
                          sifelse(is.null(star),g$star,star[i]))
            }
        } else if (name == "geom_abline") {
            xl <- sifelse(p$logx,log10(xlimits),xlimits)
            yl <- sifelse(p$logy,log10(ylimits),ylimits)
            x <- xl
            y <- g$intercept+g$slope*xl
            if (y[1] < yl[1]) {
                x[1] <- x[1] + (yl[1]-y[1])/g$slope
                y[1] <- yl[1]
            } else if (y[1] > yl[2]) {
                x[1] <- x[1] + (yl[2]-y[1])/g$slope
                y[1] <- yl[2]
            }
            if (y[2] < yl[1]) {
                x[2] <- x[2] - (y[2]-yl[1])/g$slope
                y[2] <- yl[1]
            } else if (y[2] > yl[2]) {
                x[2] <- x[2] - (y[2]-yl[2])/g$slope
                y[2] <- yl[2]
            }
            p <- psline(p, cx(p,x,FALSE), cy(p,y,FALSE), g$par)
        } else if (name == "geom_hline") {
            p <- psline(p, cx(p,xlimits), cy(p,c(g$yintercept,g$yintercept)), g$par)
        } else if (name == "geom_vline") {
            p <- psline(p, cx(p,c(g$xintercept,g$xintercept)), cy(p,ylimits), g$par)
        } else if (name == "geom_errorbar") {
            x    <- g[["x"]]
            y    <- g[["y"]]
            ymin <- g[["ymin"]]
            ymax <- g[["ymax"]]
            x <- cx(p,x$data) + g$dodge
            if (!is.null(ymin) && !is.null(ymax)) {
                ymin <- cy(p,ymin$data)
                ymax <- cy(p,ymax$data)
                for (i in 1:length(x)) {
                    p <- psline(p, c(x[i],x[i]), c(ymin[i],ymax[i]), g$par)
                    p <- psline(p, c(x[i]-g$width,x[i]+g$width), c(ymin[i],ymin[i]), g$par)
                    p <- psline(p, c(x[i]-g$width,x[i]+g$width), c(ymax[i],ymax[i]), g$par)
                }
            } else if (!is.null(ymin) && !is.null(y)) {
                ymin <- cy(p,ymin$data)
                y    <- cy(p,y$data)
                for (i in 1:length(x)) {
                    p <- psline(p, c(x[i],x[i]), c(ymin[i],y[i]), g$par)
                    p <- psline(p, c(x[i]-g$width,x[i]+g$width), c(ymin[i],ymin[i]), g$par)
                }
            } else if (!is.null(y) && !is.null(ymax)) {
                y    <- cy(p,y$data)
                ymax <- cy(p,ymax$data)
                for (i in 1:length(x)) {
                    p <- psline(p, c(x[i],x[i]), c(y[i],ymax[i]), g$par)
                    p <- psline(p, c(x[i]-g$width,x[i]+g$width), c(ymax[i],ymax[i]), g$par)
                }
            } else {
                stop("geom_errorbar: no valid data available")
            }
        } else if (name == "geom_hist") {
            cy0 <- cy(p,0)
            dxy <- p$linewidth/2
            for (i in 1:(length(x)-1)) p <- psframe(p, c(x[i]-dxy,x[i+1]+dxy), c(cy0-dxy,y[i]+dxy), par=g$par, star=g$star)
        } else if (name == "geom_legend") {
            p <- pplegend(p, g$s, g$par, g$position, g$dx, g$dy, g$w, g$labelsep)
        } else if (name == "geom_grid") {
            p <- ppgrid(p, g$par, g$background)
        }
    }

    if (p$psttoeps) p <- endP2E(p)

    p$geoms <- NULL

    p
}
