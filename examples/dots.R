library(PSTricks)

## definitions in pstricks-dots.tex
## there are aliases but also a few not mentioned in pst-user.pdf
## two TeX commands: \newpsfontdot and \newpsfontdotH depending on parameters

## basically the starred versions are not needed except perhaps with
## efficient color specifications

## with these dotstyles and parameters, all of R's base plot pch
## symbols can be created

dotstyles <- c("+", "Add", "B+", "BoldAdd", # 4
               "x", "times", "Mul", # 7
               "bullet", "Bullet", "*", # 10
               "o", "Circle", "Bo", "BoldCircle", # 14
               "oplus", "Oplus", "Oplus*", "SolidOplus", "BoldOplus", # 19
               "otimes", "Otimes", "BoldMul", "BoldOtimes", "SolidOtimes", # 24
               "asterisk", "Asterisk", "Asterisk*", "SolidAsterisk", "BoldAsterisk", "Basterisk", # 30
               "Bar", "|", "BoldBar", "B|", # 34
               "triangle", "Btriangle", "Triangle", "BoldTriangle", "triangle*", "SolidTriangle", # 40
               "square", "square*", "SolidSquare", "Bsquare", "Square", "BoldSquare", # 46
               "diamond", "Diamond", "BoldDiamond", "Bdiamond", "diamond*", "SolidDiamond", # 52
               "pentagon", "Bpentagon", "BoldPentagon", "Pentagon", "pentagon*", "SolidPentagon", # 58
               "Hexagon", "SolidHexagon", "BoldHexagon", # doubly defined? # 51
               "Octogon", "Octogon*", "BoldOctogon", "SolidOctogon") # 65

p <- PSTricks(engine="lualatex") # with pdflatex newpages between pictures may be needed

for (k in 1:3) {

p <- pppicture(p)

p <- psset(p,"dotsize=10pt, linecolor=green, fillcolor=red")

n <- length(dotstyles)

xmin <- 2
dx <- 10

ymax <- 27
ymin <- 1
dy <- (ymax-ymin) / (n/2)

x <- xmin
y <- ymax

for (i in 1:n)
{
    if (k==1) {
        p <- psdot(p, x, y, par=paste0("dotstyle=",dotstyles[i]))
    } else if (k==2) {
        p <- psdot(p, x, y, par=paste0("dotstyle=",dotstyles[i]), star=T)
    } else {
        p <- psdot(p, x, y, par=paste0("dotstyle=",dotstyles[i],",dotscale=1.5 0.75, dotangle=30"))
    }
    p <- uput(p, x+1, y, dotstyles[i], refangle='r')
    y <- y - dy
    if (y < ymin) {
        x <- x + dx
        y <- ymax
    }
}

}

p
