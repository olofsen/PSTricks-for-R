% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoms.R
\name{geom_line}
\alias{geom_line}
\title{Connect Observations using Lines}
\usage{
geom_line(p, mapping = NULL, data = NULL, par = NULL, dodge = 0, star = FALSE)
}
\arguments{
\item{p}{The PSTricks object.}

\item{mapping}{Aesthetic mapping from column names to \code{x} and \code{y}.}

\item{data}{Data frame with coordinates of the observations.}

\item{par}{PSTricks parameter string.}

\item{dodge}{Horizontal offset.}

\item{star}{Flag to indicate starred version.}
}
\value{
The updated PSTricks object.
}
\description{
Connect Observations using Lines
}
\examples{
geom_line(PSTricks(),aes(x=xdata,y=ydata),data.frame(xdata=c(4,0,2),ydata=c(2,1,0)),
    "linewidth=2pt,linearc=.25,arrows=->")
# Note that the names in the data frame determine the axis label names by default
# and that a default `pppicture()` is called automatically
}
\seealso{
\code{\link[=psline]{psline()}} for the base version.
}
