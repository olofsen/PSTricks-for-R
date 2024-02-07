#' Draw an X or Y Axis
#' @param p The PSTricks object.
#' @param xory A character 'x' or 'y' designating which axis to draw.
#' @param lims A vector with two elements, the minimum and maximum values for the axis.
#' @param label The label to show at the middle of the axis.
#' @param labsep The distance between the tickmark labels and the label.
#' @param secondary A flag to indicate that a secondary (at the other side) axis should be drawn.
#' @param noshow A flag to indicate that values should be scaled with respect to the axis, but that the axis should not be drawn.
#' @return The updated PSTricks object, with attributes `xtpos` and `ytpos` added for `ppgrid()`.
#' @export
#' @examples
#' p <- pppicture(PSTricks(),16,9) %>%
#'     ppticks('x',6,3) %>%
#'     ppticks('y',6,4) %>%
#'     ppaxis('x',c(1,6),"wt") %>%
#'     ppaxis('y',c(10,35),"mpg") ;
#'     psdots(p,cx(p,mtcars$wt),cy(p,mtcars$mpg))
#' # Note that p has to have valid axes before using `cx()` or `cy()`

ppaxis <- function(p, xory, lims, label="label", labsep=NULL,
                   secondary=FALSE, noshow=FALSE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    if (xory!='x' && xory!='y') stop(paste0("unknown axis designator `",xory,"`"))

    if (length(lims)!=2) stop("lims needs to be a vector of length 2")
    xymin <- lims[1]
    xymax <- lims[2]

    x0y0 <- adjx0y0(p, xory, secondary)
    x0 <- x0y0[1]
    y0 <- x0y0[2]

    hx <- p$hx
    hy <- p$hy

    logxy <- sifelse(xory=='x', p$logx, p$logy)

    if (logxy) {
        if (xymin<=0 || xymax<=0) stop("extrema need to be larger than zero with log axes")
        xymin <- log10(xymin)
        xymax <- log10(xymax)
    }

    if (isTRUE(all.equal(xymin,xymax))) {
        if (!isTRUE(all.equal(xymin,0))) {
            ibase <- round(log10(abs(xymin)))
            xybase <- 10^ibase
        } else {
            xybase <- 1
        }
        xymin <- xymin - xybase
        xymax <- xymax + xybase
    }

    if (xory == 'x') {
        tl <- p$xticks$ticklength
        tli <- p$xticks$ticklengthi
        if (is.null(tli)) tli <- tl
        if (is.null(logxy)) logxy <- p$logx
        nticks <- p$xticks$nticks
        mticks <- p$xticks$mticks
        nolabels <- p$xticks$nolabels
        labels <- p$xticks$labels
        rotation <- p$xticks$rotation
        if (tli > 0) {
            x <- c(x0, x0, x0+hx, x0+hx)
            if (secondary) {
                y <- c(y0-tli, y0, y0, y0-tli)
            } else {
                y <- c(y0+tli, y0, y0, y0+tli)
            }
        } else {
            x <- c(x0, x0+hx)
            y <- c(y0, y0)
        }
    } else if (xory == 'y') {
        tl <- p$xticks$ticklength
        tli <- p$xticks$ticklengthi
        if (is.null(tli)) tli <- tl
        if (is.null(logxy)) logxy <- p$logy
        nticks <- p$yticks$nticks
        mticks <- p$yticks$mticks
        nolabels <- p$yticks$nolabels
        labels <- p$yticks$labels
        rotation <- p$yticks$rotation
        if (tli > 0) {
            if (secondary) {
                x <- c(x0-tli, x0, x0, x0-tli)
                y <- c(y0, y0, y0+hy, y0+hy)
            } else {
                x <- c(x0+tli, x0, x0, x0+tli)
            }
            y <- c(y0, y0, y0+hy, y0+hy)
        } else {
            x <- c(x0, x0)
            y <- c(y0, y0+hy)
        }
    }

## major tickmarks

    xyrem <- 0
    delta <- 1e-6

    if (logxy) {
        xybase <- 1
        if (xymin <= xymax) {
            imin <- ceiling(xymin-delta)
            imax <- floor(xymax+delta)
        } else {
            imin <- floor(xymin+delta)
            imax <- ceiling(xymax-delta)
        }
        if ((xymin < xymax) && (imin > imax)) {
            if (abs(imin-xybase-xymin) < abs(imax+xybase-xymax)) {
                xymin <- imax <- imin <- imin - xybase
            } else {
                xymax <- imin <- imax <- imax + xybase
            }
        } else if ((xymin > xymax) && (imin < imax)) {
            if (abs(imin+xybase-xymin) < abs(imax-xybase-xymax)) {
                xymin <- imax <- imin <- imin + xybase
            } else {
                xymax <- imin <- imax <- imax - xybase
            }
        }
        mticks <- 8
    } else {
        if (nticks==0) {
##            xybase <- .pretty(c(xymin,xymax),bounds=FALSE)$unit # R > 4.2.0
            xybase <- pretty(c(xymin,xymax))
            xybase <- xybase[2] - xybase[1]
            sxybase <- as.integer( abs(xybase) / 10^floor(log10(abs(xybase))) )
            #            1 2 3 4 5 6 7 8 9
            mtickss <- c(3,3,2,3,4,2,6,3,2) # sxybase cannot be 10
            mticks <- mtickss[sxybase]
            if (xymin <= xymax) {
                imin <- ceiling(xymin/xybase-delta)
                imax <- floor(xymax/xybase+delta)
            } else {
                imin <- floor(xymin/xybase+delta)
                imax <- ceiling(xymax/xybase-delta)
            }
        } else {
            xybase <- (xymax - xymin) / (nticks - 1) # minimum is 2
            imin <- round(xymin/xybase)
            imax <- round(xymax/xybase)
            xyrem <- xymin - imin*xybase
        }
    }

    if (xory == 'x') {
        p$xmin <- xymin
        p$xmax <- xymax
        p$xa <- hx / (xymax - xymin);
        p$xb <- x0 - p$xa*xymin;
    } else if (xory =='y') {
        p$ymin <- xymin
        p$ymax <- xymax
        p$ya <- hy / (xymax - xymin);
        p$yb <- y0 - p$ya*xymin;
    }

    irange <- imin:imax

    if ((xory=='x' && p$xticks$extlabs) || (xory=='y' && p$yticks$extlabs)) {
        if (!isTRUE(all.equal(xymin,imin*xybase+xyrem))) {
            irange <- c((xymin-xyrem)/xybase,irange)
        }
        if (!isTRUE(all.equal(xymax,imax*xybase+xyrem))) {
            irange <- c(irange,(xymax-xyrem)/xybase)
        }
    }

    if (xory == 'x') {
        p$xtpos <- x0 + hx*(irange*xybase-xymin)/(xymax-xymin)
    } else {
        p$ytpos <- y0 + hy*(irange*xybase-xymin)/(xymax-xymin)
    }

    if (noshow) return(p)

## the axis

    if (tli > 0) {
        p <- psline(p,x,y)
    } else {
        p <- psline(p,x,y,"linecap=2")
    }

    for (i in irange) {
        if (xory == 'x') {
            xy <- i*xybase + xyrem
            x <- x0 + hx*(xy-xymin)/(xymax-xymin)
            if (logxy) xy <- 10^xy
            if (secondary) {
                p <- psline(p, c(x,x), c(y0,y0+tl))
                if (!nolabels) {
                    if (is.null(labels)) {
                        p <- uput(p, x, y0+tl, paste(xy), 'u')
                    } else {
                        p <- uput(p, x, y0+tl, labels[i-imin+1], 'u', rotation)
                    }
                }
            } else {
                p <- psline(p, c(x,x), c(y0,y0-tl))
                if (!nolabels) {
                    if (is.null(labels)) {
                        p <- uput(p, x, y0-tl, paste(xy), 'd')
                    } else {
                        p <- uput(p, x, y0-tl, labels[i-imin+1], 'd', rotation)
                    }
                }
            }
        } else {
            xy <- i*xybase + xyrem
            y <- y0 + hy*(xy-xymin)/(xymax-xymin)
            if (logxy) xy <- 10^xy
            if (secondary) {
                p <- psline(p, c(x0,x0+tl), c(y,y))
                if (!nolabels) {
                    if (is.null(labels)) {
                        p <- uput(p, x0+tl, y, paste(xy), 'r')
                    } else {
                        p <- uput(p, x0+tl, y, labels[i-imin+1], 'r', rotation)
                    }
                }
            } else {
                p <- psline(p, c(x0,x0-tl), c(y,y))
                if (!nolabels) {
                    if (is.null(labels)) {
                        p <- uput(p, x0-tl, y, paste(xy), 'l')
                    } else {
                        p <- uput(p, x0-tl, y, labels[i-imin+1], 'l', rotation)
                    }
                }
            }
        }
    }

## minor tick marks

    if (xymin <= xymax) {
        irange <- (imin-1):(imax+1)
    } else {
        irange <- (imax-1):(imin+1)
    }

    if (mticks > 0) {
        for (i in irange) {
            if (xory == 'x') {
                for (j in 1:mticks) {
                    if (logxy) {
                        xy <- log10((j+1)*10^i)
                    } else {
                        xy <- i*xybase + j*xybase/(mticks+1) + xyrem
                    }
                    x <- x0 + hx*(xy-xymin)/(xymax-xymin)
                    if (secondary) {
                        if (x>=x0 && x<=x0+hx) p <- psline(p, c(x,x), c(y0,y0+tl/2))
                    } else {
                        if (x>=x0 && x<=x0+hx) p <- psline(p, c(x,x), c(y0,y0-tl/2))
                    }
                }
            } else if (xory == 'y') {
                for (j in 1:mticks) {
                    if (logxy) {
                        xy <- log10((j+1)*10^i)
                    } else {
                        xy <- i*xybase + j*xybase/(mticks+1) + xyrem
                    }
                    y <- y0 + hy*(xy-xymin)/(xymax-xymin)
                    if (secondary) {
                        if (y>=y0 && y<=y0+hy) p <- psline(p, c(x0,x0+tl/2), c(y,y))
                    } else {
                        if (y>=y0 && y<=y0+hy) p <- psline(p, c(x0,x0-tl/2), c(y,y))
                    }
                }
            }
        }
    }

## labels

    if (label!="") {
        if (is.null(labsep)) {
            if (xory =='x') {
                labsep = p$xlabsep
            } else {
                labsep = p$ylabsep
            }
        }
        if (xory == 'x') {
            if (secondary) {
                p <- uput(p, x0+hx/2,y0+labsep, label, 'u')
            } else {
                p <- uput(p, x0+hx/2,y0-labsep, label, 'd')
            }
        } else if (xory == 'y') {
            if (secondary) {
                p <- uput(p, x0+labsep,y0+hy/2, label, 'r', "270")
            } else {
                p <- uput(p, x0-labsep,y0+hy/2, label, 'l', "90")
            }
        }
    }

    p
}
