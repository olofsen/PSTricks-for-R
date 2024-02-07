## 30 Nodes (bindings for pst-node.tex)

#' Put Stuff in a Box at a Node
#' @param p The PSTricks object.
#' @param name The name of the node.
#' @param stuff Stuff to put in a box at the node.
#' @param refpoint The reference point (see [rput()]).
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     rput(8,4,paste("\\Large",rnode(,"A","sp"),"\\hskip 2cm",rnode(,"B","Bit"))) %>%
#'     ncline("A","B")

rnode <- function(p=NULL, name, stuff, refpoint=NULL)
{
    ppbuild(match.call()[[1]],
            opt=refpoint, arg1=name, arg2=stuff, p=p)
}

#' Put Stuff in a Box at a Node
#' @rdname crnode
#' @param p The PSTricks object.
#' @param name The name of the node.
#' @param stuff Stuff to put in a box at the node.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'    rput(8,4,paste("\\Large",Rnode(,"A","sp"),"\\hskip 2cm",Rnode(,"B","Bit"))) %>%
#'    ncline("A","B")

Rnode <- function(p=NULL, name, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=name, arg2=stuff, star=star, p=p)
}

#' Create Zero-dimensional Node
#' @param p The PSTricks object.
#' @param x,y Coordinates of the node.
#' @param name The name of the node.
#' @return The updated PSTricks object.
#' @seealso [cnode()] for an example.
#' @export

pnode <- function(p=NULL, x=NULL, y=NULL, name)
{
    ppbuild(match.call()[[1]],
            x, y, arg1=name, p=p)
}

#' Create Circle Node
#' @param p The PSTricks object.
#' @param x,y Coordinates of the node.
#' @param radius Radius of the circle.
#' @param name The name of the node.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     cnode(0,1,0.25,"A") %>%
#'     pnode(3,0,"B") %>%
#'     ncline("A","B",arrows="<-")

cnode <- function(p=NULL, x=NULL, y=NULL, radius, name, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arg1=radius, arg2=name, star=star, p=p)
}

#' Create Circle Node
#' @rdname ccnode
#' @param p The PSTricks object.
#' @param x,y Coordinates of the node.
#' @param name The name of the node.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     psset("radius=0.1") %>%
#'     Cnode(0,1,"A") %>%
#'     pnode(3,0,"B") %>%
#'     ncline("A","B",arrows="<-")

Cnode <- function(p=NULL, x=NULL, y=NULL, name, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arg1=name, star=star, p=p)
}

#' Put Stuff in a Circle
#' @param p The PSTricks object.
#' @param name The name of the node.
#' @param stuff Stuff to put in a box at the node.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [ovalnode()] for an example.
#' @export

circlenode <- function(p=NULL, name, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=name, arg2=stuff, star=star, p=p)
}

#' Put Stuff in a Circle
#' @param p The PSTricks object.
#' @param x,y Coordinates of the node.
#' @param name The name of the node.
#' @param stuff Stuff to put in a box at the node.
#' @param par PSTricks parameter string.
#' @param angle Angle to put the stuff with.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     cnodeput(,,"A","X",angle=45)

cnodeput <- function(p=NULL, x=NULL, y=NULL, name, stuff, par=NULL, angle=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, angle, name, stuff, star=star, p=p)
}

#' Put Stuff in an Oval
#' @param p The PSTricks object.
#' @param name The name of the node.
#' @param stuff Stuff to put in a box at the node.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     rput(8,4,paste(circlenode(,"A","Circle"),"and",ovalnode(,"B","Oval"))) %>%
#'     ncbar("A","B","angle=90")

ovalnode <- function(p=NULL, name, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=name, arg2=stuff, star=star, p=p)
}

#' Put Stuff in a Diamond
#' @param p The PSTricks object.
#' @param name The name of the node.
#' @param stuff Stuff to put in a box at the node.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [trinode()] for an example.
#' @export

dianode <- function(p=NULL, name, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=name, arg2=stuff, star=star, p=p)
}

#' Put Stuff in a Triangle
#' @param p The PSTricks object.
#' @param name The name of the node.
#' @param stuff Stuff to put in a box at the node.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     rput(0,3,dianode(,"A","Diamond"),"tl") %>%
#'     rput(4,0,trinode(,"B","Triangle","trimode=L"),"br") %>%
#'     nccurve("A","B","angleA=-135,angleB=90")

trinode <- function(p=NULL, name, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=name, arg2=stuff, star=star, p=p)
}

#' Create a Dot Node
#' @param p The PSTricks object.
#' @param x,y Coordinates of the node.
#' @param name The name of the node.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     dotnode(,,"A","dotstyle=triangle*,dotscale=2 1") %>%
#'     dotnode(3,2,"B","dotstyle=+") %>%
#'     ncline("A","B","nodesep=3pt")

dotnode <- function(p=NULL, x=NULL, y=NULL, name, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arg1=name, star=star, p=p)
}

#' Create a Frame Node
#' @param p The PSTricks object.
#' @param x,y Optional coordinates of the center.
#' @param name The name of the node.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     fnode(,,"A") %>%
#'     fnode(2,2,"B","framesize=1 5pt",TRUE) %>%
#'     ncline("A","B","nodesep=3pt")

fnode <- function(p=NULL, x=NULL, y=NULL, name, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arg1=name, star=star, p=p)
}


## 31 Node connections

#' Draw a Line Between Two Nodes
#' @param p The PSTricks object.
#' @param nodeA,nodeB Names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the coil.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     rput(0,0,rnode(,"A","Idea 1"),"bl") %>%
#'     rput(4,3,rnode(,"B","Idea 2"),"tr") %>%
#'     ncline("A","B","nodesep=3pt","<->")

ncline <- function(p=NULL, nodeA, nodeB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=arrows, arg1=nodeA, arg2=nodeB, star=star, p=p)
}

#' Draw an Arc Between Two Nodes
#' @param p The PSTricks object.
#' @param nodeA,nodeB Names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the coil.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     cnodeput(0,0,"A","X") %>%
#'     cnodeput(3,2,"B","Y") %>%
#'     psset("nodesep=3pt") %>%
#'     ncarc("A","B",arrows="->") %>%
#'     ncarc("B","A",arrows="->")

ncarc <- function(p=NULL, nodeA, nodeB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=arrows, arg1=nodeA, arg2=nodeB, star=star, p=p)
}

#' Draw Line Segments Between Two Nodes
#' @param p The PSTricks object.
#' @param nodeA,nodeB Names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the coil.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     rput(0,3,rnode(,"A",psframebox(,"Node A")),"tl") %>%
#'     rput(4,0,ovalnode(,"B","Node B"),"br") %>%
#'     ncdiag("A","B","angleA=-90,angleB=90,arm=.5,linearc=.2")

ncdiag <- function(p=NULL, nodeA, nodeB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=arrows, arg1=nodeA, arg2=nodeB, star=star, p=p)
}

#' Draw Line Segments Between Two Nodes
#' @param p The PSTricks object.
#' @param nodeA,nodeB Names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the coil.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     cnode(0,4,"12pt","a") %>%
#'     rput(3,5,rnode(,"b","H"),"l") %>%
#'     rput(3,3,rnode(,"c","T"),"l") %>%
#'     ncdiagg("b","a","angleA=180,armA=1.5,nodesepA=3pt") %>%
#'     ncdiag("c","a","angleA=180,armA=1.5,armB=0,nodesepA=3pt")

ncdiagg <- function(p=NULL, nodeA, nodeB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=arrows, arg1=nodeA, arg2=nodeB, star=star, p=p)
}

#' Draw Line Segments Between Two Nodes
#' @param p The PSTricks object.
#' @param nodeA,nodeB Names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the coil.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     rput(8,4,paste0(rnode(,"A","Connect")," some ",rnode(,"B","words"),"!")) %>%
#'     ncbar("A","B","nodesep=3pt,angle=-90","<-**") %>%
#'     ncbar("A","B","nodesep=3pt,angle=70")

ncbar <- function(p=NULL, nodeA, nodeB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=arrows, arg1=nodeA, arg2=nodeB, star=star, p=p)
}

#' Draw Line Segments Between Two Nodes
#' @param p The PSTricks object.
#' @param nodeA,nodeB Names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the coil.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     rput(0,3,rnode(,"A",psframebox(,"Node A")),"tl") %>%
#'     rput(4,0,ovalnode(,"B","Node B"),"br") %>%
#'     ncangle("A","B","angleA=-90,angleB=90,armB=1cm")

ncangle <- function(p=NULL, nodeA, nodeB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=arrows, arg1=nodeA, arg2=nodeB, star=star, p=p)
}

#' Draw Line Segments Between Two Nodes
#' @param p The PSTricks object.
#' @param nodeA,nodeB Names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the coil.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     rput(0,4,rnode(,"A",psframebox(,"Node A")),"tl") %>%
#'     rput(4,0,ovalnode(,"B","Node B"),"br") %>%
#'     ncangles("A","B","angleA=-90,armA=1cm,armB=.5cm,linearc=.15")

ncangles <- function(p=NULL, nodeA, nodeB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=arrows, arg1=nodeA, arg2=nodeB, star=star, p=p)
}

#' Draw Line Segments Between a Node and Itself
#' @param p The PSTricks object.
#' @param nodeA,nodeB Names of the node.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the coil.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     rnode("a",psframebox(,"\\Huge A loop")) %>%
#'     ncloop("a","a","angleB=180,loopsize=1,arm=.5,linearc=.2","->")

ncloop <- function(p=NULL, nodeA, nodeB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=arrows, arg1=nodeA, arg2=nodeB, star=star, p=p)
}

#' Draw a Bezier Curve between Two Nodes
#' @param p The PSTricks object.
#' @param nodeA,nodeB Names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the coil.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     rput(0,0,rnode(,"A",psframebox(,"Node A")),"bl") %>%
#'     rput(4,3,ovalnode(,"B","Node B"),"tr") %>%
#'     nccurve("A","B","angleB=180")

nccurve <- function(p=NULL, nodeA, nodeB, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=arrows, arg1=nodeA, arg2=nodeB, star=star, p=p)
}

#' Draw a Circle between a Node and Itself
#' @param p The PSTricks object.
#' @param node Name of the node.
#' @param radius Radius of the circle.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the coil.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     rnode("A","\\textbf{back}") %>%
#'     nccircle("A",".7cm","nodesep=3pt","->")

nccircle <- function(p=NULL, node, radius, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg=arrows, arg1=node, arg2=radius, star=star, p=p)
}

#' Enclose Two Nodes in a Box
#' @param p The PSTricks object.
#' @param nodeA,nodeB Names of the nodes.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     rput(.5,0,rnode(,"A","Idea 1"),"bl") %>%
#'     rput(3.5,2,rnode(,"B","Idea 2"),"tr") %>%
#'     ncbox("A","B","nodesep=.5cm,boxsize=.6,linearc=.2,linestyle=dashed")

ncbox <- function(p=NULL, nodeA, nodeB, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=nodeA, arg2=nodeB, star=star, p=p)
}

#' Enclose Two Nodes in Curved Box
#' @param p The PSTricks object.
#' @param nodeA,nodeB Names of the nodes.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     rput(.5,0,rnode(,"A","1"),"bl") %>%
#'     rput(3.5,2,rnode(,"B","2"),"tr") %>%
#'     ncarcbox("A","B","nodesep=.2cm,boxsize=.4,linearc=.4,arcangle=50")

ncarcbox <- function(p=NULL, nodeA, nodeB, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=nodeA, arg2=nodeB, star=star, p=p)
}

#' Draw a Line Between Two Nodes
#' @param p The PSTricks object.
#' @param x,y Coordinates of the line segment.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     pcline(c(3,6),c(4,9))

pcline <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw a Bezier Curve Between Two Nodes
#' @param p The PSTricks object.
#' @param x,y Coordinates or names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     pccurve(c(3,6),c(4,9))

pccurve <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw an Arc Between Two Nodes
#' @param p The PSTricks object.
#' @param x,y Coordinates or names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     pcarc(c(3,6),c(4,9))

pcarc <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw Line Segments Between Two Nodes
#' @param p The PSTricks object.
#' @param x,y Coordinates or names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     pcbar(c(3,6),c(4,9))

pcbar <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw Line Segments Between Two Nodes
#' @param p The PSTricks object.
#' @param x,y Coordinates or names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     pcdiag(c(3,6),c(4,9))

pcdiag <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw Line Segments Between Two Nodes
#' @param p The PSTricks object.
#' @param x,y Coordinates or names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     pcdiagg(c(3,6),c(4,9))

pcdiagg <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw Line Segments Between Two Nodes
#' @param p The PSTricks object.
#' @param x,y Coordinates or names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     pcangle(c(3,6),c(4,9))

pcangle <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw Line Segments Between Two Nodes
#' @param p The PSTricks object.
#' @param x,y Coordinates or names of the nodes.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     pcangles(c(3,6),c(4,9))

pcangles <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Draw Line Segments Between a Node and Itself
#' @param p The PSTricks object.
#' @param x,y Coordinates or Name of the Node.
#' @param par PSTricks parameter string.
#' @param arrows Arrows at the end of the line.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     pcloop(c(3,6),c(4,9))

pcloop <- function(p=NULL, x, y, par=NULL, arrows=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, arrows, star=star, p=p)
}

#' Enclose Two Nodes in a Box
#' @param p The PSTricks object.
#' @param x,y Coordinates or names of the nodes.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     pcbox(c(3,6),c(4,9))

pcbox <- function(p=NULL, x, y, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, star=star, p=p)
}

#' Enclose Two Nodes in Curved Box
#' @param p The PSTricks object.
#' @param x,y Coordinates or names of the nodes.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     pcarcbox(c(3,6),c(4,9))

pcarcbox <- function(p=NULL, x, y, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            x, y, par, star=star, p=p)
}


## 32 Node connection labels: I

#' Put Label on Line
#' @param p The PSTricks object.
#' @param stuff The label to put on the line.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     cnode(0,4,".5cm","root") %>%
#'     cnode(3,5.5,"4pt","A",star=TRUE) %>%
#'     cnode(3,4,"4pt","B",star=TRUE) %>%
#'     cnode(3,2.5,"4pt","C",star=TRUE) %>%
#'     psset("nodesep=3pt") %>%
#'     ncline("root","A") %>%
#'     naput("above") %>%
#'     ncline("root","B") %>%
#'     ncput("on",star=TRUE) %>%
#'     ncline("root","C") %>%
#'     nbput("below")

ncput <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=stuff, star=star, p=p)
}

#' Put Label above Line
#' @param p The PSTricks object.
#' @param stuff The label to put on the line.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [ncput()] for an example.
#' @export

naput <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=stuff, star=star, p=p)
}

#' Put Label below Line
#' @param p The PSTricks object.
#' @param stuff The label to put on the line.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [ncput()] for an example.
#' @export

nbput <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=stuff, star=star, p=p)
}

#' Define Short Form Characters
#' @param p The PSTricks object.
#' @param char1 Short form character for `naput`.
#' @param char2 Short form character for `nbput`.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     cnode(0,4,".5cm","root") %>%
#'     cnode(3,5.5,"4pt","A",star=TRUE) %>%
#'     cnode(3,2.5,"4pt","C",star=TRUE) %>%
#'     psset("nodesep=3pt,shortput=nab") %>%
#'     MakeShortNab("+","-") %>%
#'     ppappend(paste0(ncline(,"root","A"),"+{$x$}")) %>%
#'     ppappend(paste0(ncline(,"root","C"),"-{$y$}"))
#' # so short forms are not elegantly implemented

MakeShortNab <- function(p=NULL, char1, char2)
{
    ppbuild(match.call()[[1]],
            arg1=char1, arg2=char2, p=p)
}


## 33 Node connection labels: II

#' Put Stuff on Line
#' @param p The PSTricks object.
#' @param stuff The label to put on the line.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [thput()] for an example.
#' @export

tvput <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=stuff, star=star, p=p)
}

#' Put Stuff on Line
#' @param p The PSTricks object.
#' @param stuff The label to put on the line.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' PSTricks(engine="lualatex",pstpkgs="pst-node") %>%
#'     ppappend("\\[") %>%
#'     ppappend("\\setlength{\\arraycolsep}{1.1cm}") %>%
#'     ppappend("\\begin{array}{cc}") %>%
#'     ppappend(paste(Rnode(,"a","(X-A)"),"&",Rnode(,"b","A"),"\\\\[1.5cm]")) %>%
#'     ppappend(paste(Rnode(,"c","x"),"&",Rnode(,"d","\\tilde{X}"))) %>%
#'     ppappend("\\end{array}") %>%
#'     psset("nodesep=5pt,arrows=->") %>%
#'     everypsbox("\\scriptstyle") %>%
#'     ncline("a","c") %>% tlput("r") %>%
#'     ncline("a","b") %>% taput("u") %>%
#'     ncline("c","d","linestyle=dashed") %>% tbput("b") %>%
#'     ncline("b","d") %>% trput("s") %>% ppappend("\\]")
#' # Note: no pppicture because of array

tlput <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=stuff, star=star, p=p)
}

#' Put Stuff on Line
#' @param p The PSTricks object.
#' @param stuff The label to put on the line.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [tlput()] for an example.
#' @export

trput <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=stuff, star=star, p=p)
}

#' Put Stuff on Line
#' @param p The PSTricks object.
#' @param stuff The label to put on the line.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' PSTricks(engine="lualatex",pstpkgs="pst-node") %>%
#'     ppappend("\\[") %>%
#'     ppappend("\\setlength{\\arraycolsep}{1.1cm}") %>%
#'     ppappend("\\begin{array}{cc}") %>%
#'     ppappend(paste(Rnode(,"a","(X-A)"),"&",Rnode(,"b","A"),"\\\\[1.5cm]")) %>%
#'     ppappend(paste(Rnode(,"c","x"),"&",Rnode(,"d","\\tilde{X}"))) %>%
#'     ppappend("\\end{array}") %>%
#'     psset("nodesep=5pt,arrows=->") %>%
#'     everypsbox("\\scriptstyle") %>%
#'     ncline("a","c") %>% thput("h") %>%
#'     ncline("a","b") %>% thput("h") %>%
#'     ncline("b","d") %>% tvput("v") %>%
#'     ncline("c","d") %>% tvput("v") %>%
#'     ppappend("\\]")

thput <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=stuff, star=star, p=p)
}

#' Put Stuff on Line
#' @param p The PSTricks object.
#' @param stuff The label to put on the line.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [tlput()] for an example.
#' @export

taput <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=stuff, star=star, p=p)
}

#' Put Stuff on Line
#' @param p The PSTricks object.
#' @param stuff The label to put on the line.
#' @param par PSTricks parameter string.
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @seealso [tlput()] for an example.
#' @export

tbput <- function(p=NULL, stuff, par=NULL, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=stuff, star=star, p=p)
}

#' Define Short Form Characters
#' @param p The PSTricks object.
#' @param char1 Short form character for `taput`.
#' @param char2 Short form character for `tbput`.
#' @param char3 Short form character for `tlput`.
#' @param char4 Short form character for `trput`.
#' @return The updated PSTricks object.
#' @seealso See [MakeShortNab()] for how to use short forms.
#' @export

MakeShortTablr <- function(p=NULL, char1, char2, char3, char4)
{
    ppbuild(match.call()[[1]],
            arg1=char1, arg2=char2, arg3=char3, arg4=char4, p=p)
}


## 34 Attaching labels to nodes

#' Attach Label to Node
#' @param p The PSTricks object.
#' @param name The name of the node.
#' @param stuff The label to put on the line.
#' @param par PSTricks parameter string.
#' @param refangle The reference angle (see [uput()]).
#' @param star Flag to indicate starred version.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' pppicture(PSTricks(pstpkgs="pst-node"),c(-2,14),c(-2,10),par="showgrid=true") %>%
#'     rput(4,0,ovalnode(,"B","Node B"),"br") %>%
#'     rput(0,3,rnode(,"A",psframebox(,"Node A")),"tl") %>%
#'     nput("A",paste0(psarcn(,0,0,".4cm",0,-70),
#'         uput(,0,0,"\\texttt{angleA}",-35,labelsep=".4cm")),"labelsep=0",-70) %>%
#'     ncangle("A","B","angleA=-70,angleB=90,armB=1cm,linewidth=1.2pt") %>%
#'     ncput(psframe(,c(0,.35),c(0,.35),"dimen=middle"),"nrot=:U,npos=1")

nput <- function(p=NULL, name, stuff, par=NULL, refangle, star=FALSE)
{
    ppbuild(match.call()[[1]],
            opt=par, arg1=refangle, arg2=name, arg3=stuff, star=star, p=p)
}


## 35 Mathematical diagrams and graphs

## \psmatrix and \endpsmatrix not implemented, but can be written using ppappend
