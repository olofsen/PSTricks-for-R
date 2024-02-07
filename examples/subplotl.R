library(PSTricks)

p <- PSTricks(landscape=TRUE)

nx <- 6
ny <- 2
width <- 2

x <- 28
y <- yaspect(x, nx=nx, ny=ny, width=2) # so that aspect=1 will be achieved

p <- pppicture(p, x, y, par="showgrid=true")

p <- psframe(p, c(0,p$x), c(0,p$y))

j <- 0

f <- function(p,i)
{
    p <- ppsubplot(p, nx,ny, i, width=width) # so this takes actually two subplots in the x direction
    j <<- j+1
    p <- psframe(p, c(p$x0,p$x0+p$dx), c(p$y0,p$y0+p$dy), par="linestyle=dotted")
    p <- ppaxis(p, 'x', c(0,1), label='xxx')
    p <- ppaxis(p, 'y', c(0,1), label='yyy')
    p <- pptitle(p, paste("{\\Large ",intToUtf8(64+j),":",j,"}"))
    p
}

p <- p %>%
    f(1) %>%
    f(3) %>%
    f(5) %>%
    f(8) %>% # shifts (half) a subplot
    f(10)

p
