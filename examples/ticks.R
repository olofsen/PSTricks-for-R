library(PSTricks)

## demonstrate axis layout and autoscaling

## extlabs

p <- PSTricks() %>%
    ppsetmargins(0.8) %>%
    pppicture(20,26,mtcars,aes(x=wt,y=mpg)) %>% # otherwise default pppicture by geoms
    ppylabsep(0.8) %>%
    psset("dotstyle=Bo")

##

p <- ppsubplot(p,2,4) %>%
    geom_dots() %>%
    xlab("Weight") %>%
    ylab("Miles per gallon") %>%
    pptitle("{\\large A}: automatic limits and ticks")

## labels are remembered

p <- ppsubplot(p) %>%
    geom_dots() %>%
    xlim(1,6) %>%
    ylim(10,35) %>%
    pptitle("{\\large B}: now manual limits")

p <- ppsubplot(p) %>%
    geom_dots() %>%
    xticks(6,1) %>%
    yticks(6,1) %>%
    pptitle("{\\large C}: and manual ticks")

## with manual setting of ticks when limits are calculated,
## the following may happen
## autoscaling does not determine nice values at the end of the axes,
## and these usually have to be given explicitly

p <- ppsubplot(p) %>%
    geom_dots() %>%
    lims() %>%
    pptitle("{\\large D}: same manual ticks but automatic limits")

p <- ppsubplot(p) %>%
    ticks(extlabs=TRUE) %>% # also sets determination of ticks to automatic
    geom_dots() %>%
    pptitle("{\\large E}: automatic ticks and labels at limits")

## log variants
## subdivisions not numbered, because that may be many, and in this
## case a log scale is not that meaningful
## automatically a power of 10 is included
## to have at least some number at one of the ticks

p <- ppsubplot(p) %>%
    ticks(extlabs=FALSE) %>%
    ppsetlogxy() %>%
    geom_dots() %>%
    pptitle("{\\large F}: automatic limits and ticks with log axes")

p <- ppsubplot(p) %>%
    ppsetlogxy() %>%
    geom_dots() %>%
    xlim(1,10) %>%
    ylim(10,100) %>%
    pptitle("{\\large G}: now manual limits")

## extlabs at both sides not currently possible

## mtcars$wt <- round(exp(mtcars$wt)) # but it is with a larger range

p <- ppsubplot(p,data=mtcars) %>%
    ticks(extlabs=TRUE) %>%
    geom_dots() %>%
    lims() %>%
    pptitle("{\\large H}: automatic and labels at limits")

##

p

## so the default (A) is ok but
## with xlim() / ylim() / lims() the axes may look better (B)
## with xticks() / yticks() / ticks() (F) the number of major and minor tickmarks
## may be overridden (C)
