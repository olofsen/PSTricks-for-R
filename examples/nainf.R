library(PSTricks)

nxaxes=2
nyaxes=2

data <- list(x=c(1,2,3,4),
             y1=c(-2,2,-2,2),
             y2=c(NA,2,-2,2),
             y3=c(NA,2,-Inf,Inf))

p <- PSTricks(engine="lualatex") %>% pppicture() %>% ppsetsecondaryy()

p <- ppsubplot(p,2,4,nxaxes=nxaxes,nyaxes=nyaxes,data=data) %>%
    geom_dots(aes(y=y1))

## NAs are dropped with a warning

p <- ppsubplot(p,nxaxes=nxaxes,nyaxes=nyaxes) %>%
    geom_dots(aes(y=y2))

## -Inf at bottom x axis, below y axis

p <- ppsubplot(p,nxaxes=nxaxes,nyaxes=nyaxes) %>%
    geom_dots(aes(y=y3))

## -Inf at top x axis, above y axis

p <- ppsubplot(p,nxaxes=nxaxes,nyaxes=nyaxes) %>%
    ppsetsecondaryx() %>%
    geom_dots(aes(y=y3))


## log variants

p <- ppsubplot(p,nxaxes=nxaxes,nyaxes=nyaxes) %>%
    ppsetlogy() %>%
    ppsetprimaryx() %>%
    geom_dots(aes(y=y1))

p <- ppsubplot(p,nxaxes=nxaxes,nyaxes=nyaxes) %>%
    geom_dots(aes(y=y2))

## -Inf is not plotted

p <- ppsubplot(p,nxaxes=nxaxes,nyaxes=nyaxes) %>%
    geom_dots(aes(y=y3))

p <- ppsubplot(p,nxaxes=nxaxes,nyaxes=nyaxes) %>%
    ppsetsecondaryx() %>%
    geom_dots(aes(y=y3))

p
