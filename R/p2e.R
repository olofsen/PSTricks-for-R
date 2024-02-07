#' Start PSTtoEPS Feature
#' @param p The PSTricks object.
#' @param fileplot Flag to indicate `cat`ed values will be used for `fileplot`.
#' @return The updated PSTricks object.
#' @export

startP2E <- function(p, fileplot=FALSE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$filpath <- tempfile("PE",tmpdir=p$config$tmpdir)
    p$P2Enam <- paste0(p$filpath,".eps")
    if (fileplot) {
        p <- ppappend(p,paste0("\\PSTtoEPS{", p$P2Enam, "}{\\fileplot{", p$filpath, "}"))
        sink(p$filpath)
    } else {
        p <- ppappend(p,paste0("\\PSTtoEPS{", p$P2Enam, "}{"))
    }
    p
}

#' End PSTtoEPS Feature
#' @param p The PSTricks object.
#' @param fileplot Flag to indicate `cat`ed values will be used for `fileplot`.
#' @return The updated PSTricks object.
#' @export

endP2E <- function(p, fileplot=FALSE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    if (fileplot) sink()
    p <- ppappend(p,paste0("}\\rput(", p$linewidth/2, "mm,", p$linewidth/2, "mm){\\includegraphics{", p$P2Enam, "}}%"))
    p
}
