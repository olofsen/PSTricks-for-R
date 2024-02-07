library(PSTricks)

data <- data.frame(x=c(0.1,0.9), y=c(0.9,0.1))

PSTricks(engine="pdflatex",pstpkgs="pst-eps") %>%
    pppicture(data=data,par="showgrid=true") %>%
    geom_linewidth(1) %>% # instead of `pplinewidth` so axes will be normal
    geom_line(par="linecolor=red") %>%
    ppsetpsttoeps() %>%
    geom_line(par="linecolor=green",dodge=0) # dodge!=0 shows both lines

## the `geom_line()` line will generate a separate .eps file
## With the automatically called PSTtoEPS functions:
## the `fileplot` argument in `startP2E()` is FALSE
## linewidth is used in endP2e() so that the included .eps aligns

## tmpdir is ideally /tmp, but TeX is not allowed to write there by default,
## this needs "openout_any = a" in "texmf.cnf"
