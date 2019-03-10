#-----------------------------------------------------------------------
# Definições no knitr para execução e exibição do código.

library(knitr)

opts_chunk$set(cache = FALSE,
               tidy = FALSE,
               fig.width = 7,
               fig.height = 7,
               fig.align = "center",
               eval.after= "fig.cap",
               # dev.args = list(family = "Palatino"),
               warning = FALSE,
               error = FALSE,
               message = FALSE)

#-----------------------------------------------------------------------
# Redefine opções padrão do R.

options(stringsAsFactors = FALSE)
options(width = 90)

# Define semente para todos os arquivos.
set.seed(2018)

#-----------------------------------------------------------------------
# Configurações da lattice.

library(latticeExtra)
mycol <- c("#E41A1C", "#377EB8", "#4DAF4A",
           "#984EA3", "#FF7F00", "#FFFF33")

# TODO incluir como um objeto do pacote.
# Trellis graphical style.
ps <- list(box.rectangle = list(col = 1, fill = c("gray70")),
           box.umbrella = list(col = 1, lty = 1),
           dot.symbol = list(col = 1, pch = 19),
           dot.line = list(col = "gray50", lty = 3),
           plot.symbol = list(col = 1, cex = 0.8),
           plot.line = list(col = 1),
           plot.polygon = list(col = "gray95"),
           superpose.line = list(col = mycol, lty = 1),
           superpose.symbol = list(col = mycol, pch = 1),
           superpose.region = list(col = mycol, pch = 1),
           superpose.polygon = list(col = mycol),
           strip.background = list(col = c("gray80", "gray50")),
           axis.text = list(cex = 0.8))
# trellis.par.set(ps)
# lattice.options(default.args = list(as.table = TRUE))

#-----------------------------------------------------------------------
# Legenda nas figuras.

has_capt <- require(captioner)
if (has_capt) {
    library(captioner)
    tbn_ <- captioner(prefix = "Tabela")
    fgn_ <- captioner(prefix = "Figura")
    tbl_ <- function(label) tbn_(label, display = "cite")
    fgl_ <- function(label) fgn_(label, display = "cite")
} else {
    tbn_ <- function(name = "", caption = "") identity(caption)
    fgn_ <- function(name = "", caption = "") identity(caption)
    tbl_ <- function(name = "", caption = "") identity(caption)
    fgl_ <- function(name = "", caption = "") identity(caption)
}
