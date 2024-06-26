% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subplot.R
\name{ppsubplot}
\alias{ppsubplot}
\title{Divide the Picture in Subplots}
\usage{
ppsubplot(
  p,
  nx = NULL,
  ny = NULL,
  n = NULL,
  nxaxes = 1,
  nyaxes = 1,
  ntitle = NULL,
  width = 1,
  height = 1,
  newpage = FALSE,
  data = NULL,
  mapping = NULL
)
}
\arguments{
\item{p}{The PSTricks object.}

\item{nx}{Number of plots in the x direction (if NULL, increment n automatically).}

\item{ny}{Number of plots in the y direction.}

\item{n}{Number of current plot (by default 1 if nx and ny specified).}

\item{nxaxes}{Number of x axes to make space for.}

\item{nyaxes}{Number of y axes to make space for.}

\item{ntitle}{Number of title lines to make space for.}

\item{width}{Number of subplots to occupy in the x direction.}

\item{height}{Number of subplots to occupy in the y direction.}

\item{newpage}{Flag to skip remaining subplots for the current page and go to the next page.}

\item{data}{Override earlier specified data (in pppicture or ppsubplot).}

\item{mapping}{Override earlier specified mapping (in pppicture or ppsubplot).}
}
\value{
The updated PSTricks object, with respect to the attributes
\itemize{
\item x0 - The position of the x axis.
\item y0 - The position of the y axis.
\item dx - The space allocated for the subplot in the x direction.
\item dy - The space allocated for the subplot in the y direction.
\item hx - The length of the x axis.
\item hy - The length of the y axis.
\item nx - Saved nx for subsequent subplots.
\item ny - Saved ny for subsequent subplots.
\item isub - Saved n for subsequent subplots.
\item pxad - Flag to indicate that primary x axis has been drawn.
\item pyad - Flag to indicate that primary y axis has been drawn.
\item sxad - Flag to indicate that secondary x axis has been drawn.
\item syad - Flag to indicate that secondary y axis has been drawn.
}
}
\description{
Divide the Picture in Subplots
}
\details{
Subsequent coordinates are relative to (p$x0,p$y0),
so possibly different from (0,0).
Plot parameters such as limits, ticks, and labels are not reset to default values.
}
\examples{
pppicture(PSTricks(),data=mtcars) \%>\%
    ppsubplot(2,3,data=mtcars,mapping=aes(x=wt,y=mpg)) \%>\%
    geom_dots() \%>\%
    ppsubplot() \%>\%
    geom_dots(aes(x=wt,y=cyl))
}
