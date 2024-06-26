% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pstricks.R
\name{psdblframebox}
\alias{psdblframebox}
\title{Put Stuff in a Box with a Double Frame}
\usage{
psdblframebox(p = NULL, stuff, par = NULL, star = FALSE)
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
Put Stuff in a Box with a Double Frame
}
\examples{
pppicture(PSTricks(),16,9) \%>\%
    rput(8,4,psdblframebox(,"\\\\parbox[c]{6cm}{\\\\raggedright
        A double frame is drawn with the gap between lines equal to \\\\texttt{doublesep}}"))
}
