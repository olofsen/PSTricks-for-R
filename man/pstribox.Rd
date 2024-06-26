% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pstricks.R
\name{pstribox}
\alias{pstribox}
\title{Put Stuff in a Triangle Box}
\usage{
pstribox(p = NULL, stuff, par = NULL, star = FALSE)
}
\arguments{
\item{p}{The PSTricks object.}

\item{stuff}{The stuff to put in the box.}

\item{par}{PSTricks parameter string.}

\item{star}{Flag to indicate starred version.}
}
\value{
The updated PSTricks object.
}
\description{
Put Stuff in a Triangle Box
}
\examples{
pppicture(PSTricks(),16,9) \%>\%
    rput(8,4,pstribox(,"\\\\Large\\\\textbf{Begin}","trimode=R,framesep=5pt"))
}
