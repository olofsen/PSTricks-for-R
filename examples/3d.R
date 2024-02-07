library(PSTricks)

pstThreeDCoor <- function(p=NULL, par=NULL)
{
    ppbuild(match.call()[[1]],
            par, p=p)
}

pstThreeDLine <- function(p=NULL, x, y, z, par=NULL)
{
    ppbuild3D(match.call()[[1]],
            x, y, z, par, p=p)
}

PSTricks(pstpkgs="pst-3dplot") %>%
    pppicture(c(-2.5,2.5),c(-2.5,2.5)) %>%
    pstThreeDCoor("xMin=-2,xMax=2,yMin=-2,yMax=2,zMin=-2,zMax=2") %>%
    pstThreeDLine(c(1,0), c(0,1), c(0,2), "linewidth=3pt,linecolor=blue,arrows=->")
