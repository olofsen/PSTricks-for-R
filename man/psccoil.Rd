% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pst-coil.R
\name{psCoil}
\alias{psCoil}
\title{Draw PSTricks Coil}
\usage{
psCoil(p = NULL, angle1, angle2, par = NULL, star = FALSE)
}
\arguments{
\item{p}{The PSTricks object.}

\item{angle1, angle2}{First and last angles of the coil.}

\item{par}{PSTricks parameter string.}

\item{star}{Flag to indicate starred version.}
}
\value{
The updated PSTricks object.
}
\description{
Draw PSTricks Coil
}
\examples{
pppicture(PSTricks(pstpkgs="pst-coil"),c(-1,5),c(-1,1),par="showgrid=true") \%>\%
    psCoil(0,1440,"coilaspect=0,coilheight=1.33,coilwidth=.75,linewidth=1.5pt")
}
