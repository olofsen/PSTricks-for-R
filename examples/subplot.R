library(PSTricks)

aspect <- 0.5

p <- PSTricks(engine="pdflatex") %>%
    xyaspect(aspect,2,4) %>%
    pppicture()

p <- psframe(p, c(0,p$x), c(0,p$y))

j <- 0
for (i in 1:8) {
    if (i<3 || i>4) {
        p <- ppsubplot(p, 2,4, i)
    } else {
        p <- ppsubplot(p, 2,4, i, width=2) # occupies positions 3 and 4
    }
    if (i==6) {
        p <- ppsubplot(p, 4,8, 19) # note that we need the updated p for the frame
        p <- psframe(p, c(p$x0,p$x0+p$dx), c(p$y0,p$y0+p$dy), par="linestyle=dotted") # use x0,y0 and dx,dy from p
        p <- ppsubplot(p, 4,8, 20)
        p <- psframe(p, c(p$x0,p$x0+p$dx), c(p$y0,p$y0+p$dy), par="linestyle=dotted")
        p <- ppsubplot(p, 4,8, 23)
        p <- psframe(p, c(p$x0,p$x0+p$dx), c(p$y0,p$y0+p$dy), par="linestyle=dotted")
        p <- ppsubplot(p, 4,8, 24)
    }
    if (i!=4) { # also graphs the last subplot for n=6 / n=24 even though it gets too small
        j <- j+1
        p <- psframe(p, c(p$x0,p$x0+p$dx), c(p$y0,p$y0+p$dy), par="linestyle=dotted")
        p <- ppaxis(p, 'x', c(0,1), label='xxx')
        p <- ppaxis(p, 'y', c(0,1), label='yyy')
        p <- pptitle(p, paste("{\\Large ",intToUtf8(64+j),":",i,"}"))
    }
}

p
