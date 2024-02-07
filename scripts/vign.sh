#!/bin/bash

# Rscript -e "devtools::install(build_vignettes=TRUE)"

rm -rf inst

Rscript -e "devtools::build_vignettes()"

mkdir inst
mv doc inst
