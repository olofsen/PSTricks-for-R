library(PSTricks)

p <- PSTricks(engine="latex",pstpkgs="pst-plot,pst-eps") %>%
    pppicture(16,9) %>%
    psgrid(3,5,"gridlabels=0") %>%
    psaxes(3,5)

## fileplot's 'par' argument is not available, so

p <- psset(p,"linecolor=blue")

p <- startP2E(p,TRUE)

## x y coordinates

cat("0.5 0.5\n")
cat("2.5 3.5\n")

p <- endP2E(p,TRUE)

p
