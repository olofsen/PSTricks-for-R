#' Define Major and Minor Tickmarks at the X Axis
#' @param p The PSTricks object.
#' @param nticks Number of tickmarks; if nticks=0, pretty tickmarks will be determined automatically.
#' @param mticks Number of minor tickmarks.
#' @param nolabels Flag to indicate that no labels should be printed.
#' @param extlabs Flag to indicate that labels at axis extrema should be printed (however labels cannot be used).
#' @param labels List of labels instead of numbers to print at the tickmarks.
#' @param rotation The rotation for the labels at the tickmarks.
#' @param ticklength The length of the ticks.
#' @param ticklengthi - The inward length of the ticks (default same as outward).
#' @return The updated PSTricks object.
#' @export
#' @examples
#' PSTricks() %>%
#'     geom_dots(aes(x=wt,y=mpg),mtcars) %>%
#'     xlim(0,6) %>%
#'     xticks(3,2)

ppxticks <- function(p, nticks=0, mticks=0, nolabels=FALSE, extlabs=FALSE, labels=NULL, rotation=0, ticklength=0.2, ticklengthi=NULL)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    if (nticks == 0) p$xlim <- NULL
    p$xticks <- list(nticks=nticks, mticks=mticks, nolabels=nolabels, extlabs=extlabs, labels=labels,
                     rotation=rotation, ticklength=ticklength, ticklengthi=ticklengthi)
    p
}

#' Define Major and Minor Tickmarks at the Y Axis
#' @param p The PSTricks object.
#' @param nticks Number of tickmarks; if nticks=0, pretty tickmarks will be determined automatically.
#' @param mticks Number of minor tickmarks.
#' @param nolabels Flag to indicate that no labels should be printed.
#' @param extlabs Flag to indicate that labels at axis extrema should be printed (however labels cannot be used).
#' @param labels List of labels instead of numbers to print at the tickmarks.
#' @param rotation The rotation for the labels at the tickmarks.
#' @param ticklength The length of the ticks.
#' @param ticklengthi - The inward length of the ticks (default same as outward).
#' @return The updated PSTricks object.
#' @export
#' @examples
#' PSTricks() %>%
#'     geom_dots(aes(x=wt,y=mpg),mtcars) %>%
#'     ylim(10,35) %>%
#'     yticks(6,0)

ppyticks <- function(p, nticks=0, mticks=0, nolabels=FALSE, extlabs=FALSE, labels=NULL, rotation=0, ticklength=0.2, ticklengthi=NULL)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")

    if (nticks == 0) p$ylim <- NULL
    p$yticks <- list(nticks=nticks, mticks=mticks, nolabels=nolabels, extlabs=extlabs, labels=labels,
                     rotation=rotation, ticklength=ticklength, ticklengthi=ticklengthi)
    p
}

## aliases

#' @rdname ppxticks
#' @export
xticks <- ppxticks

#' @rdname ppyticks
#' @export
yticks <- ppyticks


#' Define Major and Minor Tickmarks at the Axes
#' @param p The PSTricks object.
#' @param x,y Lists with number of major and minor tickmarks.
#' @param nolabels Flag to indicate that no labels should be printed.
#' @param extlabs Flag to indicate that labels at axis extrema should be printed (however labels cannot be used).
#' @param labels List of labels instead of numbers to print at the tickmarks.
#' @param rotation The rotation for the labels at the tickmarks.
#' @param ticklength The length of the ticks.
#' @param ticklengthi - The inward length of the ticks (default same as outward).
#' @return The updated PSTricks object.
#' @export
#' @examples
#' PSTricks() %>%
#'     geom_dots(aes(x=wt,y=mpg),mtcars) %>%
#'     lims(c(1,6),c(10,35)) %>%
#'     ticks(c(6,0),c(6,1))

ticks <- function(p, x=0, y=0, nolabels=FALSE, extlabs=FALSE, labels=NULL, rotation=0,
                    ticklength=0.2, ticklengthi=NULL)
{
    p %>%
        ppxticks(x[1], sifelse(length(x)>=2,x[2],0), nolabels, extlabs, labels, rotation,
                    ticklength, ticklengthi) %>%
        ppyticks(y[1], sifelse(length(y)>=2,y[2],0), nolabels, extlabs, labels, rotation,
                    ticklength, ticklengthi)
}

#' Define Major and Minor Tickmarks at X or Y Axis
#' @details
#' To be used with `ppaxis()`.
#' @param p The PSTricks object.
#' @param xory A character 'x' or 'y' designating which axis to draw.
#' @param nticks Number of tickmarks; if nticks=0, pretty tickmarks will be determined automatically.
#' @param mticks Number of minor tickmarks.
#' @param nolabels Flag to indicate that no labels should be printed.
#' @param extlabs Flag to indicate that labels at axis extrema should be printed (however labels cannot be used).
#' @param labels List of labels instead of numbers to print at the tickmarks.
#' @param rotation The rotation for the labels at the tickmarks.
#' @param ticklength The length of the ticks.
#' @param ticklengthi - The inward length of the ticks (default same as outward).
#' @seealso [ppaxis()] for an example.
#' @return The updated PSTricks object.
#' @export

ppticks <- function(p, xory, nticks=0, mticks=0, nolabels=FALSE, extlabs=FALSE, labels=NULL, rotation=0,
                    ticklength=0.2, ticklengthi=NULL)
{
    sifelse(xory=='x',
            ppxticks(p, nticks, mticks, nolabels, extlabs, labels, rotation, ticklength, ticklengthi),
            ppyticks(p, nticks, mticks, nolabels, extlabs, labels, rotation, ticklength, ticklengthi))
}
