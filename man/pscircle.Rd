% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pstricks.R
\name{pscircle}
\alias{pscircle}
\title{Draw PSTricks Circle}
\usage{
pscircle(p = NULL, x = NULL, y = NULL, radius, par = NULL, star = FALSE)
}
\arguments{
\item{p}{The PSTricks object.}

\item{x, y}{Coordinates of the center of the circle.}

\item{radius}{Radius of the circle.}

\item{par}{PSTricks parameter string.}

\item{star}{Flag to indicate starred version.}
}
\value{
The updated PSTricks object.
}
\description{
Draw PSTricks Circle
}
\examples{
pppicture(PSTricks(),c(-1,2),c(-1,2),par="showgrid=true") \%>\%
    pscircle(.5,.5,1.5,"linewidth=2pt")
}
