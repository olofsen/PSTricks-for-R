## R to PSTricks bindings

## 2 Color

#' Define New Gray Scale
#' @param p The PSTricks object.
#' @param color The name of the new gray scale.
#' @param num The scale value (0 is black and 1 is white).
#' @return The updated PSTricks object.
#' @export
#' @examples
#' newgray(,"gray10",0.1)

newgray <- function(p=NULL, color, num)
{
    ppbuild(match.call()[[1]],
            arg=color, arg1=num, p=p)
}

#' Define New RGB Color
#' @param p The PSTricks object.
#' @param color The name of the new color.
#' @param num1,num2,num3 The red-green-blue specification (0 is dark and 1 is light).
#' @return The updated PSTricks object.
#' @export
#' @examples
#' newrgbcolor(,"mycolor",0.1,0.2,0.3)

newrgbcolor <- function(p=NULL, color, num1, num2, num3)
{
    ppbuild(match.call()[[1]],
            arg=color, arg1=paste(num1, num2, num3), p=p)
}

## Perhaps there is a better location for the following
## added function

#' Define New RGB Color(s) from R Color Specification(s)
#' @param p The PSTricks object.
#' @param names R color names.
#' @param values Color values to parse.
#' @return The updated PSTricks object.
#' @importFrom grDevices col2rgb
#' @export
#' @examples
#' ppnewrgbcolor(,"blue") # p==NULL works for one color only

ppnewrgbcolor <- function(p=NULL, names, values=NULL)
{
    if (is.null(values)) values <- names
    rgb <- round(col2rgb(values)/255,3)
    for (i in 1:length(names)) {
        p <- newrgbcolor(p, names[i], rgb[,i][[1]], rgb[,i][[2]], rgb[,i][[3]])
    }
    p
}

#' Define New HSB Color
#' @param p The PSTricks object.
#' @param color The name of the new color.
#' @param num1,num2,num3 The hue-saturation-brightness specification (between 0 and 1).
#' @return The updated PSTricks object.
#' @export
#' @examples
#' newhsbcolor(,"mycolor",0.1,0.2,0.3)

newhsbcolor <- function(p=NULL, color, num1, num2, num3)
{
    ppbuild(match.call()[[1]],
            arg=color, arg1=paste(num1, num2, num3), p=p)
}

#' Define New CMYK Color
#' @param p The PSTricks object.
#' @param color The name of the new color.
#' @param num1,num2,num3,num4 The cyan-magenta-yellow-black specification (between 0 and 1).
#' @return The updated PSTricks object.
#' @export
#' @examples
#' newcmykcolor(,"mycolor",0.1,0.2,0.3,0.4)

newcmykcolor <- function(p=NULL, color, num1, num2, num3, num4)
{
    ppbuild(match.call()[[1]],
            arg=color, arg1=paste(num1, num2, num3, num4), p=p)
}


## 3 Setting graphics parameters

#' Set Any Native PSTricks Option
#' @param p The PSTricks object.
#' @param s A string with `par=value` specifications (comma separated).
#' @return The updated PSTricks object.
#' @export
#' @examples
#' psset(,"linewidth=0.1mm")

psset <- function(p=NULL, s)
{
    s <- paste0("\\psset{", s, "}")

    if (is.null(p)) return(s)

    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    ppappend(p, s)
}


## 4 Dimensions, coordinates and angles

#' Set Unit for Angles
#' @param p The PSTricks object.
#' @param degrees The number of units in a circle.
#' @return The updated PSTricks object.
#' @seealso [ppsetpolar()].
#' @export

degrees <- function(p, degrees=360)
{
    p$degrees <- degrees
    p
}


## 6 Lines and polygons

#' Draw PSTricks Line
#' @param p The PSTricks object.
#' @param x,y Coordinates of the line segment(s).
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [geom_line()] for the version with scaling.
#' @export
#' @examples
#' pppicture(PSTricks(),4,2,par="showgrid=true") %>%
#'     psline(c(4,0,2),c(2,1,0),"linewidth=2pt,linearc=.25","->")

psline <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw PSTricks Line Segment
#' @param p The PSTricks object.
#' @param x,y Coordinates of the line segment.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),2,1,par="showgrid=true") %>%
#'     qline(c(0,2),c(0,1))

qline <- function(p=NULL, x, y)
{
    ppbuild(match.call()[[1]],
            x, y, p=p)
}

#' Draw PSTricks Polygon
#' @param p The PSTricks object.
#' @param x,y Coordinates of the line segment(s).
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),4,2,par="showgrid=true") %>%
#'     pspolygon(c(0,1),c(2,2),"linewidth=1.5pt") %>%
#'     pspolygon(c(1,1,4,4),c(0,2,0,2),"linearc=.2", star=TRUE)

pspolygon <- function(p=NULL, x, y, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, star=star, p=p)
}

#' Draw PSTricks Frame
#' @param p The PSTricks object.
#' @param x,y Coordinates of the frame.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),4,2,par="showgrid=true") %>%
#'     psframe(4,2,"linewidth=2pt,framearc=.3,fillstyle=solid,fillcolor=lightgray") %>%
#'     psframe(c(1,2),c(.5,1.5),"linecolor=white",star=TRUE)

psframe <- function(p=NULL, x, y, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, star=star, p=p)
}

## A later addition

#' Draw PSTricks Text Frame
#' @param p The PSTricks object.
#' @param x,y Coordinates of the frame.
#' @param text Text to display in the frame.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),8,6,par="showgrid=true") %>%
#'     psTextFrame(c(0,4),c(0.5,1.5),"Hallo","linecolor=lightgray,ref=l")

psTextFrame <- function(p=NULL, x, y, text, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arg1=text, star=star, p=p)
}

#' Draw PSTricks Diamond
#' @param p The PSTricks object.
#' @param x,y Coordinates of the diamond.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),4,2,par="showgrid=true") %>%
#'     psdiamond(c(2,1.5),c(1,1),"framearc=.3,fillstyle=solid,fillcolor=lightgray")

psdiamond <- function(p=NULL, x, y, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, star=star, p=p)
}

#' Draw PSTricks Triangle
#' @param p The PSTricks object.
#' @param x,y Coordinates of the triangle.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),4,2,par="showgrid=true") %>%
#'     pstriangle(c(2,4),c(.5,1),"gangle=10", star=TRUE)

pstriangle <- function(p=NULL, x, y, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, star=star, p=p)
}


## 7 Arcs, circles and ellipses

#' Draw PSTricks Circle
#' @param p The PSTricks object.
#' @param x,y Coordinates of the center of the circle.
#' @param radius Radius of the circle.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),c(-1,2),c(-1,2),par="showgrid=true") %>%
#'     pscircle(.5,.5,1.5,"linewidth=2pt")

pscircle <- function(p=NULL, x=NULL, y=NULL, radius, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arg1=radius, star=star, p=p)
}

#' Draw PSTricks Circle
#' @param p The PSTricks object.
#' @param x,y Coordinates of the center of the circle and one point on the circle.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(engine="latex"),8,8,par="showgrid=true") %>%
#'     pscircleOA(c(6,4),c(4,4)) %>%
#'     pscircleOA(c(4,4),c(6,4),"linecolor=blue") %>%
#'     pscircleOA(c(3,4),c(5,4),"linewidth=2pt,linecolor=yellow") %>%
#'     pscircleOA(c(2,4),c(4,4),"opacity=0.3,linecolor=red",TRUE)

pscircleOA <- function(p=NULL, x, y, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, star=star, p=p)
}

#' Draw PSTricks Disk
#' @param p The PSTricks object.
#' @param x,y Coordinates of the center of the disk.
#' @param radius Radius of the disk.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),4,6) %>%
#'     psset("linecolor=gray") %>%
#'     qdisk(2,3, 4*2.54/72)

qdisk <- function(p=NULL, x, y, radius)
{
    ppbuild(match.call()[[1]],
            x, y, arg1=radius, p=p)
}

#' Draw PSTricks Wedge
#' @param p The PSTricks object.
#' @param x,y Coordinates of the center of and the horizontal and vertical radii.
#' @param radius Radius of the wedge.
#' @param angle1,angle2 End and start angles of the wedge.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),2,2,par="showgrid=true") %>%
#'     pswedge(0,0,2,0,70,"linecolor=gray,linewidth=2pt,fillstyle=solid")

pswedge <- function(p=NULL, x=NULL, y=NULL, radius, angle1, angle2, par, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arg1=radius, arg2=angle1, arg3=angle2, star=star, p=p)
}

#' Draw PSTricks Ellipse
#' @param p The PSTricks object.
#' @param x,y Coordinates of the center of and the horizontal and vertical radii.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),c(-1,2),c(-1,1),par="showgrid=true") %>%
#'     psellipse(c(.5,1.5),c(0,1),"fillcolor=lightgray")

psellipse <- function(p=NULL, x, y, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, star=star, p=p)
}


#' Draw PSTricks Arc
#' @param p The PSTricks object.
#' @param x,y Coordinates of the arc.
#' @param radius Radius of the arc.
#' @param angleA,angleB Start and end angles of the arc.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),3,2,par="showgrid=true") %>%
#'     psarc(1.5,1.5,1.5,215,0,"showpoints=true",star=TRUE)

psarc <- function(p=NULL, x=NULL, y=NULL, radius, angleA, angleB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, arg1=radius, arg2=angleA, arg3=angleB, star=star, p=p)
}

#' Draw PSTricks Arc Clockwise
#' @param p The PSTricks object.
#' @param x,y Coordinates of the arc.
#' @param radius Radius of the arc.
#' @param angleA,angleB End and start angles of the arc.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),4,3,par="showgrid=true") %>%
#'     ppsetpolar() %>%
#'     psline(c(4,0,4),c(50,0,10),"linewidth=2pt") %>%
#'     psarcn(0,0,3,50,10,"arcsepB=2pt",arrows="<-")

psarcn <- function(p=NULL, x=NULL, y=NULL, radius, angleA, angleB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, arg1=radius, arg2=angleA, arg3=angleB, star=star, p=p)
}

#' Draw PSTricks Elliptic Arc
#' @param p The PSTricks object.
#' @param x,y Coordinates of the elliptic arc.
#' @param angleA,angleB Start and end angles of the arc.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),c(-1,2),c(-1,1),par="showgrid=true") %>%
#'     psellipticarc(c(.5,1.5),c(0,1),215,0,"showpoints=true,arrowscale=2","->")

psellipticarc <- function(p=NULL, x, y, angleA, angleB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, arg1=angleA, arg2=angleB, star=star, p=p)
}

#' Draw PSTricks Elliptic Arc Clockwise
#' @param p The PSTricks object.
#' @param x,y Coordinates of the elliptic arc.
#' @param angleA,angleB Start and end angles of the arc.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),c(-1,2),c(-1,1),par="showgrid=true") %>%
#'     psellipticarcn(c(.5,1.5),c(0,1),0,215,"showpoints=true,arrowscale=2","<-")

psellipticarcn <- function(p=NULL, x, y, angleA, angleB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, arg1=angleA, arg2=angleB, star=star, p=p)
}


## 8 Curves

#' Draw PSTricks Bezier Curve
#' @param p The PSTricks object.
#' @param x,y Coordinates of the line segment(s).
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),4,4) %>%
#'     psbezier(c(0,1,2,4),c(0,4,1,3.5),"linewidth=2pt,showpoints=true","->")

psbezier <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}


#' Draw PSTricks Parabola
#' @param p The PSTricks object.
#' @param x,y Coordinates of the parabola.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),4,3,par="showgrid=true") %>%
#'    parabola(c(1,2), c(1,3), star=TRUE) %>%
#'    psset("xunit=.01") %>%
#'    parabola(c(400,200),c(3,0),arrows="<->")

parabola <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw PSTricks Curve
#' @param p The PSTricks object.
#' @param x,y Coordinates of the curve.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),4,2,par="showgrid=true") %>%
#'     pscurve(c(0,0.7,3.3,4,0.4),c(1.3,1.8,0.5,1.6,0.4),
#'         "showpoints=true","<->")

pscurve <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw PSTricks Extended Curve
#' @param p The PSTricks object.
#' @param x,y Coordinates of the curve.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),4,4,par="showgrid=true") %>%
#'     psecurve(c(.125,.25,.5,1,2,4,8),c(8,4,2,1,.5,.25,.125),
#'         "showpoints=true")

psecurve <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw PSTricks Closed Curve
#' @param p The PSTricks object.
#' @param x,y Coordinates of the curve.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),4,1,par="showgrid=true") %>%
#'     psccurve(c(.5,3.5,3.5,.5),c(0,1,0,1),"showpoints=true")

psccurve <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}


## 9 Dots

#' Draw PSTricks Dot
#' @param p The PSTricks object.
#' @param x,y Coordinates of the dot.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),2,2) %>%
#'     psdot(1,1)

psdot <- function(p=NULL, x=NULL, y=NULL, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, star=star, p=p)
}

#' Draw PSTricks Dots
#' @param p The PSTricks object.
#' @param x,y Coordinates of the dots.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),2,2) %>%
#'     psdots(c(0,1,2),c(1,1,1),"dotstyle=Bo")

psdots <- function(p=NULL, x, y, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, star=star, p=p)
}


## 10 Grids

#' Draw PSTricks Grid
#' @param p The PSTricks object.
#' @param x,y Coordinates of the grid.
#' @param par PSTricks parameter string.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' PSTricks() %>%
#'     pppicture(c(-2,4),c(-2,3)) %>%
#'     psgrid(c(0,-1,3), c(0,-1,2)) %>%
#'     pppicture(c(-1,3),c(-1,2)) %>%
#'     psgrid()

psgrid <- function(p=NULL, x=NULL, y=NULL, par=NULL)
{
    ppbuild(match.call()[[1]],
            x, y, par, p=p)
}


## 17 Custom graphics

#' Custom graphics
#' @param p The PSTricks object.
#' @param commands Commands to call.
#' @param par PSTricks parameter string.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' PSTricks() %>%
#'     pppicture(4,3,par="showgrid=true") %>%
#'     pscustom(paste0(pscurve(,c(0,1,2,4),c(2,2.5,1.5,3)),
#'                     pscurve(,c(4,3,2,1,0),c(1,0.5,1,0,0.5),"liftpen=1")),
#'              "linewidth=2pt,fillstyle=solid,fillcolor=gray")

pscustom <- function(p=NULL, commands, par=NULL)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=commands, p=p)
}


## 23 Pictures

#' Begin Picture Environment
#' @details Available, but see `pppicture()`.
#' @param p The PSTricks object.
#' @param x,y Coordinates of upper right corner (and optionally lower left corner).
#' @param par Parameters (see Voss' latest documentation).
#' @param star Flag to indicate that objects should be clipped with respect to the boundaries.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' PSTricks(engine="pdflatex") %>%
#'     psset("linecolor=red") %>%
#'     pspicture(1,1,"showgrid") %>%
#'     rput(0,0,
#'          paste(pspicture(,1,1,star=TRUE),
#'                psline(,c(-1,2),c(-1,2)),
#'                endpspicture()),
#'          "lb") %>%
#'     endpspicture()
#' # Example found on the internet for clipping while showing labels

pspicture <- function(p=NULL, x, y, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, star=star, p=p)
}

#' End Picture Environment
#' @param p The PSTricks object.
#' @return The updated PSTricks object.
#' @seealso [pspicture()] for an example.
#' @export

endpspicture <- function(p=NULL)
{
    ppbuild(match.call()[[1]], p=p)
}

## 24 Placing and rotating whatever

#' Put Stuff at Refpoint
#' @param p The PSTricks object.
#' @param x,y Coordinates of the stuff (may be omitted if `rotation` is present).
#' @param stuff Stuff to put at the reference point.
#' @param refpoint The reference point for the stuff.
#' @param rotation Rotation to apply to the stuff.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),c(-1,4),c(-1,4)) %>%
#'     rput(stuff=paste0(psframe(,c(-1,2),c(0,1)),
#'         rput(,2,1,"\\emph{stuff}","br","*0")),rotation=34)

rput <- function(p=NULL, x=NULL, y=NULL, stuff, refpoint=NULL, rotation=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, refpoint, rotation, stuff, star=star, p=p)
}

#' Put Stuff as Label
#' @param p The PSTricks object.
#' @param x,y Coordinates of the stuff (may be omitted if `rotation` is present).
#' @param stuff Stuff to put at the reference point.
#' @param refangle The reference angle.
#' @param rotation Rotation to apply to the stuff.
#' @param labelsep Distance between coordinates and the stuff.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),3,3) %>%
#'     qdisk(1,1,"1pt") %>%
#'     uput(1,1,"(1,1)",45)

uput <- function(p=NULL, x=NULL, y=NULL, stuff, refangle=NULL, rotation=NULL, labelsep=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, refangle, rotation, stuff, arg0=labelsep, star=star, p=p)
}


## 25 Repetition

#' Put Copies of Stuff
#' @param p The PSTricks object.
#' @param x,y Coordinates of the stuff.
#' @param n Number of copies.
#' @param stuff Stuff to put at the reference point.
#' @param angle Angle for the copies.
#' @param refpoint The reference point for the stuff.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),3,3) %>%
#'     multirput(c(.5,.3),c(0,.1),12,'*')

multirput <- function(p=NULL, x, y, n, stuff, angle=NULL, refpoint=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, refpoint, angle, n, stuff, star=star, p=p)
}


## 27 Framed boxes

#' Put Stuff in a Box with a Frame
#' @param p The PSTricks object.
#' @param stuff The stuff to put in the box.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),3,2) %>%
#'     pspolygon(c(0,3,3,2),c(0,0,2,2),"fillcolor=gray,fillstyle=crosshatch*") %>%
#'     rput(2,1,psframebox(,"Label","framearc=.3",star=TRUE))

psframebox <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=stuff, star=star, p=p)
}

#' Put Stuff in a Box with a Double Frame
#' @param p The PSTricks object.
#' @param stuff The stuff to put in the box.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),16,9) %>%
#'     rput(8,4,psdblframebox(,"\\parbox[c]{6cm}{\\raggedright
#'         A double frame is drawn with the gap between lines equal to \\texttt{doublesep}}"))

psdblframebox <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=stuff, star=star, p=p)
}

#' Put Stuff in a Box with a Frame and a Shadow
#' @param p The PSTricks object.
#' @param stuff The stuff to put in the box.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),16,9) %>%
#'     rput(8,4,"\\psshadowbox{\\textbf{Great Idea!!}}")

psshadowbox <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=stuff, star=star, p=p)
}

#' Draw a Circle Box
#' @param p The PSTricks object.
#' @param stuff The stuff to put in the box.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),16,9) %>%
#'     rput(8,4,pscirclebox(,"{\\begin{tabular}{c} You are \\\\ here \\end{tabular}}"))

pscirclebox <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=stuff, star=star, p=p)
}

#' Put Stuff in a Circle
#' @param p The PSTricks object.
#' @param x,y Coordinates of the center of the circle.
#' @param stuff The stuff to put in the box.
#' @param par PSTricks parameter string.
#' @param angle Rotation to apply to the stuff.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),2,1,par="showgrid=true") %>%
#'     cput(1,.5,"\\large $K_1$","doubleline=true")

cput <- function(p=NULL, x, y, stuff, par=NULL, angle=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, angle, stuff, star=star, p=p)
}

#' Put Stuff in an Oval Box
#' @param p The PSTricks object.
#' @param stuff The stuff to put in the box.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),16,9) %>%
#'     rput(8,4,paste0("\\parbox{3.75cm}{At the introductory price of ",
#'         psovalbox(,"\\$13.99","boxsep=false,linecolor=darkgray"),
#'         ", it pays to act now!}"))

psovalbox <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=stuff, star=star, p=p)
}

#' Put Stuff in a Diamond Box
#' @param p The PSTricks object.
#' @param stuff The stuff to put in the box.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),16,9) %>%
#'     rput(8,4,psdiabox(,"\\Large\\textbf{Happy?}","shadow=true"))

psdiabox <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=stuff, star=star, p=p)
}

#' Put Stuff in a Triangle Box
#' @param p The PSTricks object.
#' @param stuff The stuff to put in the box.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),16,9) %>%
#'     rput(8,4,pstribox(,"\\Large\\textbf{Begin}","trimode=R,framesep=5pt"))

pstribox <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=stuff, star=star, p=p)
}


# 28 Clipping

#' Put Stuff in a Box with Clipping
#' @param p The PSTricks object.
#' @param stuff The stuff to put in the box.
#' @param dim Distance between the box and clipping.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),16,9) %>%
#'     rput(8,4,clipbox(,"\\parbox[t][1cm][t]{2cm}{One of the best
#'         new plays I have seen all all year}",-0.1))

clipbox <- function(p=NULL, stuff, dim=NULL)
{
    ppbuild(match.call()[[1]],
            opt=dim, arg=stuff, p=p)
}


## 29 Rotation and scaling boxes

#' Rotate Box Left
#' @param p The PSTricks object.
#' @param stuff Stuff to rotate.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),16,9) %>%
#'     rput(8,4,paste("\\Large\\bfseries",
#'         rotateleft(,"Left"),rotatedown(,"Down"),rotateright(,"Right")))

rotateleft <- function(p=NULL, stuff)
{
    ppbuild(match.call()[[1]],
            arg=stuff, p=p)
}

#' Rotate Box Right
#' @param p The PSTricks object.
#' @param stuff Stuff to rotate.
#' @return The updated PSTricks object.
#' @seealso [rotateleft()] for an example.
#' @export

rotateright <- function(p=NULL, stuff)
{
    ppbuild(match.call()[[1]],
            arg=stuff, p=p)
}

#' Rotate Box Down
#' @param p The PSTricks object.
#' @param stuff Stuff to rotate.
#' @return The updated PSTricks object.
#' @seealso [rotateleft()] for an example.
#' @export

rotatedown <- function(p=NULL, stuff)
{
    ppbuild(match.call()[[1]],
            arg=stuff, p=p)
}

#' Scale Box
#' @param p The PSTricks object.
#' @param num1,num2 Numbers to scale horizontally and vertically
#' @param stuff Stuff to scale.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),16,9) %>%
#'     rput(8,4,psscalebox(,"Big and long",4,2))

psscalebox <- function(p=NULL, stuff, num1, num2)
{
    ppbuild(match.call()[[1]],
            arg=paste(num1,num2), arg1=stuff, p=p)
}

#' Scale Box To
#' @param p The PSTricks object.
#' @param x,y Width and height to scale to.
#' @param stuff Stuff to rotate.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(),16,9) %>%
#'     rput(8,4,psscaleboxto(,4,2,"Big and long"))

psscaleboxto <- function(p=NULL, x, y, stuff)
{
    ppbuild(match.call()[[1]],
            x, y, arg1=stuff, p=p)
}


## Appendix A Boxes

#' Prepend String to every `psbox`
#' @param p The PSTricks object.
#' @param s The string to prepend.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' everypsbox(PSTricks(), "\\Large")$lines[[1]]

everypsbox <- function(p, s)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    ppappend(p,paste0("\\everypsbox{",s,"}"))
}
