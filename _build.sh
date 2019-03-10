#!/bin/bash

# Removes the _site/.
echo "------------------------------------------------------------------------"
echo "Remove '_site/' directory.\n"
rm -rf _site/
# Rscript -e "library(rmarkdown); clean_site()"

# The root file.
# echo $PWD
THEROOT=$PWD
cd $THEROOT
echo "Root directory: $THEROOT."

echo "------------------------------------------------------------------------"
echo "Render Rnw files inside this directory."
echo "Files that will be rendered:\n"
# List of Rmd files.
RMDFILES=$(find . -name \*.Rmd -print)
echo "$RMDFILES" | tr " " "\n"

# # Runs rmarkdown::render() in each Rmd file down in the tree.
# for RMD in $RMDFILES; do
#     DIRNAME=`dirname "$RMD"`
#     RMDNAME=`basename "$RMD"`
#     echo $DIRNAME
#     cd $DIRNAME
#     if [ "$DIRNAME" = './slides' -o "$DIRNAME" = './tutorials' ]; then
#         echo "------------------------------------------------------------------------"
#         echo "Rendering $RMD in the $DIRNAME directory.\n"
#         Rscript -e "library(rmarkdown); render(\"$RMDNAME\")"
#     fi
#     cd $THEROOT
# done

# Renders the site.
echo "------------------------------------------------------------------------"
echo "Rendering the site.\n"
Rscript -e "library(rmarkdown); render_site()"

firefox _site/index.html

# # Remove the Rmd files.
# echo "------------------------------------------------------------------------"
# echo "Removing source files (*.Rmd).\n"
# rm -r --verbose _site/slides/*.Rmd
# rm -r --verbose _site/tutorials/*.Rmd

# Tree of the directories.
echo "------------------------------------------------------------------------"
echo "Directory tree.\n"
tree -h -F _site/ -L 1
# tree -h -F _site/slides
# tree -h -F _site/scripts
# tree -h -F _site/tutorials

# # Upload.
# echo "------------------------------------------------------------------------"
# echo "Uploading files to server.\n"
# rsync -avzp \
#       ./_site/ \
#       --progress \
#       --rsh="ssh -p$PATAXOP" "$PATAXO:~/public_html/cursoR/data-vis/"
#
# # Vist the homepage.
# echo "------------------------------------------------------------------------"
# echo "Visiting the webpage.\n"
# firefox http://leg.ufpr.br/~walmes/cursoR/data-vis
