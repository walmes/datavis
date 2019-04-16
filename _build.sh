#!/bin/bash

# Removes the _site/.
echo "------------------------------------------------------------------------"
echo "Remove '_site/' directory.\n"
rm -rf _site/
# Rscript -e "library(rmarkdown); clean_site()"

echo "------------------------------------------------------------------------"

if [ -z $1 ]
then
    echo "Rendering the site.\n"
    Rscript -e "library(rmarkdown); render_site()"
elif [ -f $1 ]
then
    echo "Rendering one file.\n"
    Rscript -e "library(rmarkdown); render_site(\"$1\")"
elif [ $1 = "_index.Rmd" ]
then
    echo "Rendering _index.Rmd.\n"
    Rscript -e "library(rmarkdown); render_site(\"_index.Rmd\")"
fi

firefox _site/index.html

# Tree of the directories.
echo "------------------------------------------------------------------------"
echo "Directory tree.\n"
tree -h -F _site/ -L 1
