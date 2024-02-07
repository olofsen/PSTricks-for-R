## There may be multiple pictures in one document,
## as an alternative for subplots

pkgload::load_all()

data <- data.frame(x=c(0.1,0.9), y=c(0.9,0.1))

p <- PSTricks()

## centering on the page is determined by the first picture
## unless switched off
## see ppnewpage() for another example

p <- p %>%
    pppicture(16,9, data, par="showgrid=true") %>%
    pplinewidth(2) %>% # this affects the axis lines!
    psline(c(7,8), c(-1,10)) %>% # this line is not clipped
    geom_line() %>% # this line is clipped by the y axis +/- margins
    ylim(0.3,0.7) %>%
    pptitle("\\Large picture 1")

## with the starred version of pspicture, the gridlabels
## are clipped...

p <- p %>%
    pppicture(16,9, data, par="showgrid=true", star=TRUE) %>%
    geom_linewidth(2) %>% # linewidth is reset, axis lines ok, works for geom_line
    psline(c(7,8), c(-1,10)) %>% # this line is clipped
    geom_line() %>%
    ylim(0.3,0.7) %>% # this line is clipped, but not by the y axis +/- margins
    pptitle("\\Large picture 2")

print(p,clean=FALSE)
