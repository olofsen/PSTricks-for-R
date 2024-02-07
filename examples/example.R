library(PSTricks)

data <- data.frame(x=c(0.1,0.9), y=c(0.9,0.1))

PSTricks() %>%
    pppicture(16,9, data, par="showgrid=true") %>%
    geom_line() %>%
    pptitle("title")
