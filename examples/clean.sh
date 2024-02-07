#!/bin/bash

rm *.tex *.log *.dvi *.aux *.ps *.pdf toeps.eps PE* *~

for F in axis geoms grid legend options picture pst-coil pst-node pst-plot pstricks subplot ticks write ; do
    pushd $F
    rm *.pdf
    popd
done
