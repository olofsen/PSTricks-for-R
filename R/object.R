#' Create a PSTricks Object
#' @param x Width of paper (default A4).
#' @param y Height of paper (default A4).
#' @param engine Engine to produce a .pdf from the output .tex file. One of "lualatex" (default), "xelatex", "pdflatex", and "latex". No pdf will be produced if the engine name is not recognized.
#' @param paper Paper size specification. One of "a4" (default) or "letter".
#' @param landscape Flag to indicate landscape paper.
#' @param center Flag to use LaTeX offsets to center pictures based on the first one.
#' @param packages Font or other packages to load (default default).
#' @param pstpkgs PSTricks packages in addition to pstricks itself (default none).
#' @param familydefault Familydefault (default \\sfdefault).
#' @param tmpdir Temporary directory for the PSTtoEPS feature.
#' @return An initial PSTricks object with attributes
#' * docOpened - A flag indicating that the LaTex document has been opened (in the `lines` attribute`).
#' * picOpened - A flag indicating that the `pspicture` PSTricks environment has been opened.
#' * paperx - The horizontal paper size in cm.
#' * papery - The vertical paper size in cm.
#' * x - The horizontal picture size in cm.
#' * y - The vertical picture size in cm.
#' * landscape - A flag indicating portrait or landscape output mode.
#' * center - A flag indicating that the `pspicture` will be centered on the paper.
#' * config - A list of configuration items (see below).
#' * lines - A list of LaTeX lines to be created.
#'
#' The configuration list may consist of the following items:
#' * engine - The engine used to process the generated .tex file.
#' * familydefault - The default font family.
#' * packages - A list of additional LaTeX packages to be used.
#' * paper - The type of paper, for example "a4" or "letter".
#' * pstpkgs - A list of additional PSTricks packages (normally only "pstricks.sty").
#' * tmpdir - The temporary directory for the `PSTtoEPS` feature.
#' * gscmd - The name of the Ghostscript executable to use (default "gs").
#' @importFrom rconfig rconfig
#' @export
#' @examples
#' names(PSTricks())

PSTricks <- function(x=NULL, y=NULL,
                     engine=c("default","lualatex","xelatex","pdflatex","latex"),
                     paper=c("default","a4","letter"),
                     landscape=FALSE,
                     center=TRUE,
                     packages=NULL,
                     pstpkgs=NULL,
                     familydefault=NULL,
                     tmpdir=".")
{
    ## a config file may override some defaults

    config <- tryCatch(rconfig::rconfig(Sys.getenv("R_RCONFIG_FILE",unset="~/pstricks.yml")),
                       error=function(w){}, warning=function(w){})

    if (engine[1] == "default") {
        config$engine <- sifelse(is.null(config$engine),"lualatex",config$engine)
    } else {
        config$engine <- engine[1]
    }
    if (paper[1] == "default") {
        config$paper <- sifelse(is.null(config$paper),"a4",config$paper)
    } else {
        config$paper <- paper[1]
    }
    if (!is.null(packages)) config$packages <- paste0(config$packages,',',packages)
    if (!is.null(pstpkgs)) config$pstpkgs <- pstpkgs
    if (!is.null(familydefault)) config$familydefault <- familydefault
    config$tmpdir <- sifelse(is.null(config$tmpdir),tmpdir,config$tmpdir)
    if (is.null(config$gscmd)) config$gscmd <- "gs"

    if (config$paper == "letter") {
        if (landscape) {
            paperx <- 11*2.54 # specifies letter landscape
            papery <- 8.5*2.54
        } else {
            paperx <- 8.5*2.54 # specifies letter portrait
            papery <- 11*2.54
        }
    } else {
        if (landscape) {
            papery <- 100/sqrt(sqrt(2))/4 # specifies A4 landscape
            paperx <- papery*sqrt(2)
        } else {
            paperx <- 100/sqrt(sqrt(2))/4 # specifies A4 portrait
            papery <- paperx*sqrt(2)
        }
    }
    if (is.null(x)) x <- floor(paperx)
    if (is.null(y)) y <- floor(papery)

    structure(list(docOpened=FALSE,
                   picOpened=FALSE,
                   paperx=x, # for clipping in `ppcoords` and landscape in `ppwrite`
                   papery=y,
                   x=x,
                   y=y,
                   landscape=landscape, # for dvips in `ppwrite`
                   center=center,
                   config=config,
                   lines=list()), class="PSTricks")
}

#' `print` a PSTricks Object
#' @param x The PSTricks object.
#' @param ... Parameters for `ppwrite`.
#' @export

print.PSTricks <- function(x, ...)
{
    invisible(ppwrite(x, ...))
}
