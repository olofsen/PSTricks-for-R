library(PSTricks)

data <- data.frame(x=c(0.1,0.9), y=c(0.9,0.1))

p <- PSTricks() %>%
    pppicture(16,9, data) %>%
    geom_line(1) %>%
    ppgeoms()

## getting `x`: mapping argument is not a valid aes()
