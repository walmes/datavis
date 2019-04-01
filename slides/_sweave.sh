#!/bin/bash

_usage() {
    cat <<EOF
    Usage:    _swaeve file [-b|-h]

      file
        Is a filename with extension *.Rnw.

    Options:

      -h    Show this message.
      -b    Run biber.

    Example:

      sh _swaeve foo.Rnw
      sh _swaeve foo.Rnw -b

EOF
}

# Prints the usage and exists.
if [ "$1" = "-h" ]
then
    _usage
    exit 1
fi

# Sweave the file and creates the *.tex.
FILE=$1
Rscript -e "require(knitr); knit(\"$FILE\")"

# Creates the *.pdf.
FILENAME=${FILE%%.*}
pdflatex $FILENAME

# Runs biber to render the references.
if [ "$2" = "-b" ]
then
    echo "Run biber."
    # Split file name from extension.
    biber $FILENAME
    pdflatex $FILENAME
    pdflatex $FILENAME
fi
