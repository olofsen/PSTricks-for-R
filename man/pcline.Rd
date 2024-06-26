% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pst-node.R
\name{pcline}
\alias{pcline}
\title{Draw a Line Between Two Nodes}
\usage{
pcline(p = NULL, x, y, par = NULL, arrows = NULL, star = FALSE)
}
\arguments{
\item{p}{The PSTricks object.}

\item{x, y}{Coordinates of the line segment.}

\item{par}{PSTricks parameter string.}

\item{arrows}{Arrows at the end of the line.}

\item{star}{Flag to indicate starred version.}
}
\value{
The updated PSTricks object.
}
\description{
Draw a Line Between Two Nodes
}
\examples{
pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") \%>\%
    pcline(c(3,6),c(4,9))
}
