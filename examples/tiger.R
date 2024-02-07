library(PSTricks)

PSTricks(engine="lualatex") %>%
    pppicture(5,5,par="showgrid=true") %>%
    psgrid(par="subgriddiv=0,gridlabels=0") %>%
    rput(0,0, "\\includegraphics[scale=0.25]{tiger.eps}", "bl")
