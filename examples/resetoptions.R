library(PSTricks)

data <- data.frame(x=c(0.1,0.9), y=c(0.9,0.1))

p <- PSTricks(engine="latex", landscape=TRUE) %>%
    pppicture(16,9, data=data, par="showgrid=true") %>%
    geom_set("linecolor=red") %>%
    geom_line() %>%
    endpppicture() %>%
    ppappend("\\newpage") %>%
    pppicture(data=data, par="showgrid=true") %>% # data is by default NULL
    geom_line() # linecolor will be reset to the default

p
