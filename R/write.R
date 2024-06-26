#' Write Assembled PSTricks Picture(s) to a File
#'
#' `ppwrite()` is used to write the assembled LaTeX document to a file.
#' It does not return the PSTricks object, as it will no longer be useful
#' (a new `PSTricks()` call is needed).
#' `ppwrite` may be called automatically by R via `print` (`print.PSTricks`).
#' @param p The PSTricks object.
#' @param filename The name of the .tex file to write the document to (by default the name of the script, or "pp" when interactive).
#' @param topdf Flag to specify if a .pdf should be generated by the engine as specified with `PSTricks()`.
#' @param crop Flag if a cropped version with name `-crop.pdf` should be created.
#' @param topng Flag to specify if the .pdf should be converted to a .png.
#' @param dsf DownScaleFactor for Ghostscript when converting to .png (resolution is 4x72=288 pixels per inch).
#' @param toeps Flag to specify if an .eps should be generated (using latex and dvips -E).
#' @param clean Flag to specify if intermediate files should be deleted after generating the .pdf.
#' @return Nothing.
#' @importFrom scriptName current_filename
#' @importFrom fs path_ext_set
#' @importFrom tools file_path_sans_ext
#' @export
#' @examples
#' ppwrite(pppicture(PSTricks(engine="pdflatex"),par="showgrid=true"))
#' # where the "pdflatex" engine is the only one showing the grid labels
#' # with a full A4 picture.

ppwrite <- function(p, filename=NULL, topdf=TRUE, crop=FALSE, topng=FALSE, dsf=4,
                    toeps=FALSE, clean=TRUE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    if (!is.null(p$geoms)) p <- ppgeoms(p)
    if (p$picOpened) p <- endpppicture(p)
    if (!p$docOpened) p <-ppopendoc(p)
    p <- ppclosedoc(p)

    if (is.null(filename)) filename <- current_filename()
    if (is.null(filename)) {
        filename <- "pp"
    } else {
        filename <- basename(filename)
    }
    filename <- fs::path_ext_set(filename, "tex")

    sink(filename)
    cat(unlist(p$lines),sep='')
    sink()

    if (!topdf && !toeps) return()

    f <- tools::file_path_sans_ext(filename)

    engine <- sifelse(toeps, "latex", p$config$engine)

    if (engine == "latex") {

        sep <- sifelse(.Platform$OS.type=="windows", '#', '=')

        if (p$landscape) {
            if (toeps) {
                tmpeps <- tempfile("EPS",tmpdir=p$config$tmpdir)
                cmds <- c(paste0("latex ",f),
                          paste0("dvips -t unknown -T ",round(p$paperx*10),"mm,",round(p$papery*10),"mm -E -o ",tmpeps," ",f))
            } else {
                cmds <- c(paste0("latex ",f),
                          paste0("dvips -t unknown -T ",round(p$paperx*10),"mm,",round(p$papery*10),"mm -o ",f,".ps ",f),
                          paste0("ps2pdf -dALLOWPSTRANSPARENCY -dAutoRotatePages",sep,"/None ",f,".ps ",f,".pdf"))
            }
        } else {
            if (toeps) {
                tmpeps <- tempfile("EPS",tmpdir=p$config$tmpdir)
                cmds <- c(paste0("latex ",f),
                          paste0("dvips -E -o ",tmpeps," ",f))
            } else {
                cmds <- c(paste0("latex ",f),
                          paste0("dvips -o ",f,".ps ",f),
                          paste0("ps2pdf -dALLOWPSTRANSPARENCY -dAutoRotatePages",sep,"/None ",f,".ps ",f,".pdf"))
            }
        }

## Combine exit codes

        status <- sapply(cmds,system)
        status <- sifelse(all(status==0),0,1)

        if (status==0 && toeps) {
            # dvips -E usually does not give a useful bounding box
            cmd <- paste0("epstool --bbox --copy ",tmpeps," ",f,".eps")
            status <- system(cmd)
            file.remove(tmpeps)
        }

        if (status==0 && clean) {
            if (toeps) {
                file.remove(paste0(f,".dvi"))
            } else {
                file.remove(paste0(f,c(".dvi",".ps")))
            }
        }

    } else if (engine == "pdflatex") {

        cmd <- paste0("pdflatex -shell-escape ",f) # gives warnings about ignoring non-pdf specials...
        status <- system(cmd)

    } else if (engine == "xelatex") {

        cmd <- paste0("xelatex -output-driver=\"xdvipdfmx -i dvipdfmx-unsafe.cfg -q -E\" ",f)
        status <- system(cmd)

    } else if (engine == "lualatex") {

        cmd <- paste0("lualatex ",f)
        status <- system(cmd)

    } else {

        return()

    }

    if (status!=0) return()

    if (clean) {
        file.remove(paste0(f,c(".tex",".log",".aux")))
    }

    if (engine == "pdflatex") {
        if (crop || topng) {
            file.rename(paste0(f,"-pics.pdf"),paste0(f,"-crop.pdf"))
        } else {
            file.remove(paste0(f,"-pics.pdf"))
        }
    } else if (!toeps) {
        if (crop || topng) {
            cmd <- paste0("pdfcrop ",f, ".pdf")
            system(cmd)
        }
    }

    if (topng && !toeps) {
        cmd <- paste0(p$config$gscmd," -q -sDEVICE=png16m -r288 -dDownScaleFactor=",dsf," -dEPSCrop -dNOPAUSE -dBATCH -dALLOWPSTRANSPARENCY -sOutputFile=",f,".png ",f,"-crop.pdf")
        system(cmd)
        file.rename(paste0(f,"-crop.pdf"),paste0(f,".pdf")) # handy for bookdown
    }

    return()
}
