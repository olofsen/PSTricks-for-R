#!/bin/bash

Rscript -e "devtools::document()"

R CMD Rd2pdf --no-preview --force -o PSTricks.pdf .
