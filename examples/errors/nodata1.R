library(PSTricks)

data <- data.frame(x=c(0.1,0.9), y=c(NA,NA))

p <- PSTricks() %>%
    pppicture(16,9, data) %>%
    geom_line() %>%
    ppgeoms()

## Error in addvar(p, g, "y", p$logy) : `y` data contain NAs only
