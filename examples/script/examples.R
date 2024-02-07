#!/usr2/local/bin/Rscript

## for example, with working directory "./examples/pstricks"
## $ ../../scripts/examples.R ../../R/pstricks.R
## it can be checked if all oneliner examples run
## note that ppwrite produces an example.pdf

library(PSTricks)
library(stringr)

args <- commandArgs(trailingOnly=TRUE)

filnam <- args[1]

name <- tools::file_path_sans_ext(basename(filnam))

raw <- readLines(filnam)

n <- length(raw)

state <- 0
j <- 0

for (i in 1:n) {
    line <- raw[i]
    if (line == "#' @examples") {
        state <- 1
    } else if (state == 1) {
        eline <- substring(line,4)
        state <- 2
    } else if (state==2 && nchar(line)>0) {
        if (substring(line,1,2)=="#'") {
            if (substring(line,4,4)!='#') eline <- paste0(eline, substring(line,4))
        } else if (substring(line,1,1)!="#") {
            p <- eval(parse(text=eline))
            if (is(p,"PSTricks")) {
                j <- j + 1
                pscmd <- word(line,1)
                print(p, paste0(sprintf("%02d",j),"-",name,"-",pscmd))
            }
            state <- 0
        }
    }
}


if (j > 1) system("pdfunite *-*.pdf all.pdf")

q("no")
