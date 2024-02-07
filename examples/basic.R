library(PSTricks)

p <- PSTricks()

p <- pppicture(p, 16, 9)

## basic style without aes, geoms, and pipes

p <- ppaxis(p, 'x', c(0,1), label='\\large x label')
p <- ppaxis(p, 'y', c(0,1), label='\\large y label')

## coordinates need to be scaled

p <- psline(p, cx(p,c(0.1,0.9)), cy(p,c(0.9,0.1)))

p <- pptitle(p, "\\Large title")

p
