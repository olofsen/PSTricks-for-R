library(PSTricks)

n <- dim(mtcars)[1]
mtcars$x <- 1:n
carnames <- paste("\\small",row.names(mtcars))

PSTricks() %>%
    pppicture(16,9,data=mtcars) %>%
    xticks(n,labels=carnames,rotation=45) %>%
    geom_dots(aes(x=x,y=mpg)) %>%
    ylab("Miles per gallon")
