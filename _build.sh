#!/bin/bash

if [ "$1" = "-h" ]
then
    echo
    echo "Usage of this Bash Script"
    echo
    echo "    sh _build.sh -h     : This help."
    echo "    sh _build.sh        : rmarkdown::render_site(\"index.Rmd\")."
    echo "    sh _build.sh <file> : rmarkdown::render_site(\"<file>\")."
    echo "    sh _build.sh site   : rmarkdown::render_site()"
    echo "    sh _build.sh clean  : rmarkdown::clean_site(preview = TRUE)"
    echo
    exit 0
fi

# Removes the _site/.
echo "------------------------------------------------------------------------"
echo "Remove '_site/' directory if it exists.\n"
[ -d "_site" ] && rm -rf _site/

# Rscript -e "library(rmarkdown); clean_site()"

echo "------------------------------------------------------------------------"

if [ -z "$1" ]
then
    echo "Rendering the \"index.Rmd\".\n"
    if [ -f "index.Rmd" ]
    then
        Rscript -e "rmarkdown::render_site('index.Rmd')"
    else
        echo "File \"index.Rmd\" does not exists."
        exit 1
    fi
else
    case "$1" in
        clean )
            echo "Files that will be removed:"
            Rscript -e "rmarkdown::clean_site(preview = TRUE)"
            echo "Wish to remove files? [y/n]"
            read opcao;  echo $opcao
            if [ $opcao = "y" ];
            then
                echo "Removes files."
                Rscript -e "rmarkdown::clean_site()"
                echo
            fi
            ;;
        site )
            echo "Renders site."
            Rscript -e "rmarkdown::render_site()"
            echo
            ;;
        * )
            echo "Renders file \"$1\"."
            if [ -f "$1" ]
            then
                Rscript -e "rmarkdown::render_site(\"$1\")"
                echo
            else
                echo "File \"$1\" does not exists."
                exit 1
            fi
            ;;
    esac
fi

# firefox _site/index.html

# Tree of the directories.
echo "------------------------------------------------------------------------"
echo "Directory tree.\n"
[ -d "_site" ] && tree -h -F _site/ -L 1
