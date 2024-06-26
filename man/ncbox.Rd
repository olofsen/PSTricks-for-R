% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pst-node.R
\name{ncbox}
\alias{ncbox}
\title{Enclose Two Nodes in a Box}
\usage{
ncbox(p = NULL, nodeA, nodeB, par = NULL, star = FALSE)
}
\arguments{
\item{p}{The PSTricks object.}

\item{nodeA, nodeB}{Names of the nodes.}

\item{par}{PSTricks parameter string.}

\item{star}{Flag to indicate starred version.}
}
\value{
The updated PSTricks object.
}
\description{
Enclose Two Nodes in a Box
}
\examples{
pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") \%>\%
    rput(.5,0,rnode(,"A","Idea 1"),"bl") \%>\%
    rput(3.5,2,rnode(,"B","Idea 2"),"tr") \%>\%
    ncbox("A","B","nodesep=.5cm,boxsize=.6,linearc=.2,linestyle=dashed")
}
