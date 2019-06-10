#!/bin/bash

# Tree of the directories.
echo "------------------------------------------------------------------------"
if [ -d "_site" ]
then
    echo "Directory tree.\n"
    tree -h -F _site/ -L 1
    # tree -h -F _site/slides
    # tree -h -F _site/scripts
    # tree -h -F _site/tutorials
else
    echo "Directory \"_site/\" does not exists."
    exit 1
fi

# Upload.
echo "------------------------------------------------------------------------"
echo "Uploading files to server.\n"
rsync -avzp \
      ./_site/ \
      --progress \
      --rsh="ssh -p$PATAXOP" "$PATAXO:~/public_html/cursoR/data-vis/"

# Vist the homepage.
echo "------------------------------------------------------------------------"
echo "Visiting the webpage.\n"
firefox http://leg.ufpr.br/~walmes/cursoR/data-vis/
