#!/bin/bash

set -e

Rscript -e "devtools::document()"

R CMD build .

R CMD INSTALL PSTricks_0.1.0.tar.gz
