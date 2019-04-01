#---- setup.R ----------------------------------------------------------

library(knitr)

opts_chunk$set(cache = FALSE,
               tidy = FALSE,
               fig.align = "center",
               fig.width = 7,
               fig.height = 4,
               fig.dim = c(7, 4),
               eval.after= "fig.cap",
               # dev.args = list(family = "Palatino"),
               warning = FALSE,
               error = FALSE,
               message = FALSE)

options(width = 72)

# ATTENTION: Only works for Rnw files. Doesn't work in Rmd.
# http://animation.r-forge.r-project.org/knitr/
knit_theme$set("bclear")
# knit_theme$set("acid")
# knit_theme$set("anotherdark")
# knit_theme$set("default")
# knit_theme$set("kellys")
# knit_theme$set("moe")
# knit_theme$set("molokai")
# knit_theme$set("nuvola")
# knit_theme$set("seashell")
# knit_theme$set("zenburn")

#-----------------------------------------------------------------------
