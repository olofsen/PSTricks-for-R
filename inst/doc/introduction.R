## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PSTricks)

## -----------------------------------------------------------------------------
p <- PSTricks() %>%
    pppicture(4,2,par="showgrid=true") %>%
    psline(c(4,0,2), c(2,1,0), "linewidth=2pt,linearc=.25", "->")

## -----------------------------------------------------------------------------
print(p, "intro-psline", topng=TRUE)

## ----echo=FALSE,fig.align="center"--------------------------------------------
knitr::include_graphics("intro-psline.png")

## ----fig.align="center"-------------------------------------------------------
x <- c(4,0,2)
y <- c(2,1,0)

p <- PSTricks() %>%
    pppicture(16,9,par="showgrid=true") %>%
    ppaxis('x', c(0,4), label='\\large x label') %>%
    ppaxis('y', c(0,2), label='\\large y label')

# p, needed for the scaling conversion functions `cx` and `cy` below,
# contains only the the necessary information after p has been assigned
# as in the previous statement

p <- p %>%
    psline(cx(p,x), cy(p,y), "linewidth=2pt,linearc=.25", "->") %>%
    pptitle("\\Large title")

print(p, "intro-basic", topng=TRUE)

## ----echo=FALSE,fig.align="center"--------------------------------------------
knitr::include_graphics("intro-basic.png")

## ----fig.align="center"-------------------------------------------------------
data <- data.frame(x=c(4,0,2),y=c(2,1,0))

PSTricks() %>%
    pppicture(16,9, data=data) %>%
    everypsbox("\\large") %>%
    geom_line(par="linewidth=2pt,linearc=.25,arrows=->") %>%
    xlab("x label") %>%
    ylab("y label") %>%
    pptitle("\\Large title") %>%
    print("intro-newstyle", topng=TRUE)

## ----echo=FALSE,fig.align="center"--------------------------------------------
knitr::include_graphics("intro-newstyle.png")

