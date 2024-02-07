library(PSTricks)

data <- data.frame(x=c(0.1,0.9), y=c(-1,-1))

p <- PSTricks() %>%
    pppicture(16,9, data) %>%
    ppsetlogy() %>%
    geom_line() %>%
    ppgeoms()

## Error in addvar(p, g, "y", p$logy) : no `y` data to plot
## values<=0 removed from `y` data
