#!/bin/bash

set -e

rm -rf ..Rcheck
rm -rf PSTricks.Rcheck

Rscript -e "devtools::document()"

R CMD build .

R CMD check PSTricks_0.1.0.tar.gz

R CMD INSTALL PSTricks_0.1.0.tar.gz

