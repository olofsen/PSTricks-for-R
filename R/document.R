#' Open the LaTex Document
#'
#' Adds lines to the `p` object to start a self-contained LaTeX document.
#' While this function is exported, it is called automatically when necessary.
#' @param p The PSTricks object.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' p <- ppopendoc(PSTricks())

ppopendoc <- function(p)
{
    if (p$docOpened) return(p)

    if (p$landscape) {
        geometryline <- "\\usepackage[margin=0pt,landscape]{geometry}"
    } else {
        geometryline <- "\\usepackage[margin=0pt]{geometry}"
    }

    if (!is.null(p$config$packages)) {
        pkgsline <- paste0("\\usepackage{",p$config$packages,",graphicx}")
    } else {
        pkgsline <- "\\usepackage{graphicx}"
    }

    if (p$config$engine == "pdflatex") {
        pstpkgsline <- "auto-pst-pdf"
    } else {
        pstpkgsline <- "pstricks"
    }

    if (!is.null(p$config$pstpkgs)) {
        pstpkgsline <- paste0("\\usepackage{",pstpkgsline,",",p$config$pstpkgs,"}")
    } else {
        pstpkgsline <- paste0("\\usepackage{",pstpkgsline,"}")
    }

    if (!is.null(p$config$familydefault)) {
        familydefaultline <- paste0("\\renewcommand{\\familydefault}{\\",p$config$familydefault,"}")
    } else {
        familydefaultline <- "\\renewcommand{\\familydefault}{\\sfdefault}"
    }

    ## numbering from pst-user.pdf

    p <- p %>% ppappendline(paste0("\\documentclass[",p$config$paper,"paper]{article}")) %>%
               ppappendline(geometryline) %>%
               ppappendline("\\usepackage[T1]{fontenc}") %>%
               ppappendline(pkgsline) %>%
               ppappendline(pstpkgsline) %>%
               ppappendline(familydefaultline) %>%
               ppappendline("\\parskip 0 cm") %>%
               ppappendline("\\parindent 0 cm") %>%
               ppappendline("\\pagestyle{empty}") %>%
               ppappendline("\\begin{document}")
    p$docOpened <- TRUE
    p
}

#' Close the LaTex Document
#'
#' Adds a line to the `p` object to finish a self-contained LaTeX document.
#' While this function is exported, it is called automatically when necessary.
#' @param p The PSTricks object.
#' @return The updated PSTricks object.
#' @export
#' @examples
#' p <- ppclosedoc(ppopendoc(PSTricks()))

ppclosedoc <- function(p)
{
    if (!p$docOpened) return(p)
    p <- ppappendline(p,"\\end{document}")
    p$docOpened <- FALSE
    p
}
