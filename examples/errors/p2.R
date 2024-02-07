library(PSTricks)

p <- PSTricks()

p %>% psline(p, 1, 1)

## p is actually both the first and second argument,
## which can give strange errors
