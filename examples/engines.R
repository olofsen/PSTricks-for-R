## based on the PSTricks "Mixed colors" example

library(PSTricks)

landscape <- FALSE
crop <- FALSE
topng <- FALSE
clean <- FALSE

ColoredOverlappedSurfaces <- function(a1,a2,a3,a4)
{
    paste0(psset(,paste0("fillcolor=",a2)),a1,psset(,paste0("fillcolor=",a4)),a3)
}

f <- function(engine)
{
    p <- PSTricks(engine=engine, landscape=landscape) %>%
        pppicture(par="showgrid=true") %>%
        pptitle(paste("\\Large",engine)) %>%
        newrgbcolor("LemonChiffron", 1, 0.98, 0.8) %>%
        newrgbcolor("LightBlue", 0.68, 0.85, 0.9) %>%
        psset("unit=2,fillstyle=solid,opacity=0.5")

    p <- rput(p, 2,12,
              ColoredOverlappedSurfaces(pscircle(,,,1),"red",psellipse(, c(0.3,1.5), c(-0.7,1)), "green"))

    p <- rput(p, 2,8,
              ColoredOverlappedSurfaces(pscircle(,,,1),"blue",psellipse(, c(0.3,1.5), c(-0.7,1)), "green"))

    p <- rput(p, 2,4,
              ColoredOverlappedSurfaces(pscircle(,,,1),"red",psellipse(, c(0.3,1.5), c(-0.7,1)), "blue"))

    p <- rput(p, 6,12,
              ColoredOverlappedSurfaces(pscircle(,,,1),"yellow",psellipse(, c(0.3,1.5), c(-0.7,1)), "LemonChiffron"))

    p <- rput(p, 6,8,
              ColoredOverlappedSurfaces(pscircle(,,,1),"red",psellipse(, c(0.3,1.5), c(-0.7,1)), "yellow"))

    p <- rput(p, 6,4,
              ColoredOverlappedSurfaces(pscircle(,,,1),"LightBlue",psellipse(, c(0.3,1.5), c(-0.7,1)), "LemonChiffron"))

    print(p, paste0("engine-",engine), crop=crop, topng=topng, clean=clean)
}

f("latex")
f("pdflatex")
f("xelatex")
f("lualatex")

