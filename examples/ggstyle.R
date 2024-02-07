library(PSTricks)

## remove the margin(s); but axis ticks and labels will be drawn
## outside of the pppicture...

PSTricks() %>%
    newrgbcolor("verylightgray",.9,.9,.9) %>%
    ppsetmargins(0) %>%
    pppicture(16,9,data=mtcars,mapping=aes(x=wt,y=mpg)) %>%
    geom_grid("linestyle=dotted,linecolor=white","verylightgray") %>%
    geom_dots() %>%
    ticks(ticklengthi=0)
