pkgload::load_all()

p <- PSTricks(pstpkgs="pst-node") %>%
    pppicture(20,20,par="showgrid=true") %>%
    everypsbox("\\Large") %>%
    psset("framearc=.3,arrowsize=5pt")

x <- 4
y <- 17
dy <- 3

p <- p %>%
    rput(x,y,rnode(,"C",psframebox(,"PSTricks()"))) %>%
    rput(x,y-dy,rnode(,"P",psframebox(,"pppicture()"))) %>%
    rput(x,y-2*dy,rnode(,"S",psframebox(,"ppsubplot()"))) %>%
    rput(x,y-3*dy,rnode(,"G",psframebox(,"pp, ps, or geom\\_ objects"))) %>%
    rput(x,y-4*dy,rnode(,"W",psframebox(,"ppwrite() or print()"))) %>%
    ncline("C","P",arrows="->") %>%
    ncline("P","S",arrows="->") %>%
    ncline("S","G",arrows="->") %>%
    ncline("G","W",arrows="->")

print(p)
