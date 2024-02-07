## 48/55 Coils and zigzags

#' Draw PSTricks Coil
#' @param p The PSTricks object.
#' @param x,y Coordinates of the coil.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the coil.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-coil"),c(-1,5),c(-1,3),par="showgrid=true") %>%
#'     pscoil(4,2,"coilarm=.5cm,linewidth=1.5pt,coilwidth=.5cm","<-|")

pscoil <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw PSTricks Coil
#' @rdname psccoil
#' @param p The PSTricks object.
#' @param angle1,angle2 First and last angles of the coil.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-coil"),c(-1,5),c(-1,1),par="showgrid=true") %>%
#'     psCoil(0,1440,"coilaspect=0,coilheight=1.33,coilwidth=.75,linewidth=1.5pt")

psCoil <- function(p=NULL, angle1, angle2, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=angle1, arg2=angle2, star=star, p=p)
}

#' Draw PSTricks Zigzag
#' @param p The PSTricks object.
#' @param x,y Coordinates of the zigzag.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the zigzag.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-coil"),c(-1,5),c(-1,1),par="showgrid=true") %>%
#'     pszigzag(4,0,"coilarm=.5,linearc=.1","<->")
#' # Note that the zigzag is drawn partly outside the pppicture.

pszigzag <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw a Coil between two Nodes
#' @param p The PSTricks object.
#' @param nodeA,nodeB Names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the coil.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-coil"),c(-1,5),c(-1,4),par="showgrid=true") %>%
#'     cnode(.5,.5,.5,"A") %>%
#'     cnode(3.5,2.5,.5,"B","fillstyle=solid,fillcolor=lightgray") %>%
#'     nccoil("A","B","coilwidth=.3","<->")
#' # Note that the `pst-node` macro package does not have to be specified.

nccoil <- function(p=NULL, nodeA, nodeB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=arrows, arg1=nodeA, arg2=nodeB, star=star, p=p)
}

#' Draw a Zigzag between two Nodes
#' @param p The PSTricks object.
#' @param nodeA,nodeB Names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the zigzag.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-coil"),c(-1,5),c(-1,4),par="showgrid=true") %>%
#'     cnode(.5,.5,.5,"A") %>%
#'     cnode(3.5,2.5,.5,"B","fillstyle=solid,fillcolor=lightgray") %>%
#'     nczigzag("A","B","coilarm=.5,linearc=.1","<->")

nczigzag <- function(p=NULL, nodeA, nodeB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=arrows, arg1=nodeA, arg2=nodeB, star=star, p=p)
}

#' Draw a Coil between two Nodes
#' @param p The PSTricks object.
#' @param x,y Coordinates or names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the coil.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-coil"),4,3,par="showgrid=true") %>%
#'     pccoil(c(.5,3.5),c(.5,2.5),"coilwidth=.3","<->")

pccoil <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw a Zigzag between two Nodes
#' @param p The PSTricks object.
#' @param x,y Coordinates or names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the zigzag.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-coil"),4,3,par="showgrid=true") %>%
#'     pczigzag(c(.5,3.5),c(.5,2.5),"coilarm=.5,linearc=.1","<->")

pczigzag <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}
