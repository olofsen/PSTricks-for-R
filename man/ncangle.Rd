% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pst-node.R
\name{ncangle}
\alias{ncangle}
\title{Draw Line Segments Between Two Nodes}
\usage{
ncangle(p = NULL, nodeA, nodeB, par = NULL, arrows = NULL, star = FALSE)
}
\arguments{
\item{p}{The PSTricks object.}

\item{nodeA, nodeB}{Names of the nodes.}

\item{par}{PSTricks parameter string.}

\item{arrows}{Arrows at the end of the coil.}

\item{star}{Flag to indicate starred version.}
}
\value{
The updated PSTricks object.
}
\description{
Draw Line Segments Between Two Nodes
}
\examples{
pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") \%>\%
    rput(0,3,rnode(,"A",psframebox(,"Node A")),"tl") \%>\%
    rput(4,0,ovalnode(,"B","Node B"),"br") \%>\%
    ncangle("A","B","angleA=-90,angleB=90,armB=1cm")
}
