#!/bin/sh

# List all `*.Rmd` files.
RMDS=$(ls *.Rmd)

echo "Rendering all files in the list."
echo $RMDS

for RMD in $RMDS;
do
    echo "----------------------------------------------------------------------"
    echo "Rendering file: $RMD."
    echo
    Rscript -e "rmarkdown::render(\"$RMD\")"
    echo
    echo "----------------------------------------------------------------------"
done
