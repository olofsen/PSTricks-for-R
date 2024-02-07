library(PSTricks)

options("warnPartialMatchDollar"=TRUE)

d <- list(x=c(1,2,3),y=c(4,5,6),z=c(9,8,7))

p <- PSTricks() %>% pppicture(16,18)

p <- ppsubplot(p, 1, 2, 1, nxa=2, nya=2)

p <- geom_line(p, data=d, par="showpoints=true")

p <- ppgeoms(p)

p <- ppsetsecondary(p, 'y')

p <- geom_line(p, aes(x=x,y=z), data=d, par="showpoints=true")

p <- ppgeoms(p)

p <- ppsetsecondary(p, 'x')

p <- geom_line(p, aes(x=y,y=z), data=d, par="showpoints=true")

p <- ppsubplot(p, nxa=2, nya=2)

#p <- ppsetprimary(p, 'x')

p <- geom_line(p, data=d, par="showpoints=true")

p
