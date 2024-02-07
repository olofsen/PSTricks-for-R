#!/bin/bash

set -e

export R_RCONFIG_FILE="/nas/PSTricks/pstricks.yml"

rm -f *.pdf

for F in *.R; do
    echo EXAMPLE $F
    Rscript $F
done

pdfunite *.pdf all.pdf

for F in axis geoms grid legend options picture pst-coil pst-node pst-plot pstricks subplot ticks write ; do
    echo EXAMPLE DIRECTORY $F
    pushd $F
    rm -f *.pdf
    ../script/examples.R ../../R/${F}.R
    popd
done
