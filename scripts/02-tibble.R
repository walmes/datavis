#-----------------------------------------------------------------------
# Manipulação e visualização de dados: a abordagem tidyverse
# http://leg.ufpr.br/~walmes/cursoR/data-vis/
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                leg.ufpr.br/~walmes · github.com/walmes
#                                        walmes@ufpr.br · @walmeszeviani
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-mar-19 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#=======================================================================
# Solução dos exercícios propostos nos slides sobre `tibble`.

#-----------------------------------------------------------------------
# Carrega o pacote.

library(tidyverse)

#-----------------------------------------------------------------------
# Criar um tibble a partir do objeto `precip`.

str(precip)

precip %>%
    enframe(name = "city", value = "preciptation")

#-----------------------------------------------------------------------
# Criar um tibble a partir do objeto `cars`.

str(cars)

cars %>%
    as_tibble()

#-----------------------------------------------------------------------
# Criar um tibble a partir do objeto `mtcars`.

str(mtcars)

mtcars %>%
    as_tibble()

#-----------------------------------------------------------------------
# Criar um tibble a partir do objeto `anscombe` com as variáveis `x` e
# `y` empilhadas.

str(anscombe)

with(anscombe,
     tibble(x = c(x1, x2, x3, x4),
            y = c(y1, y2, y3, y4),
            group = rep(1:4, each = 11))
)

#-----------------------------------------------------------------------
# Criar um tibble a partir do objeto `WorldPhones` com os valores
# empilhados.

str(WorldPhones)
c(WorldPhones)

tibble(local = rep(colnames(WorldPhones), each = nrow(WorldPhones)),
       ano = rep(rownames(WorldPhones), times = ncol(WorldPhones)),
       valor = c(WorldPhones))

#-----------------------------------------------------------------------
# Criar um tibble a partir do objeto `HairEyeColor` com os valores
# empilhados.

str(HairEyeColor)

as.data.frame(HairEyeColor) %>%
    as_tibble()

#-----------------------------------------------------------------------
# Criar um tibble a partir dos dados disponíveis no gráficos em
# http://www.vizwiz.com/2016/06/data-scientists-need-alteryx.html.

tibble(tipo = rep(c("tempo", "enfado"), each = 6),
       tarefa = rep(c(1:6), times = 2),
       valor = c(3, 60, 19, 9, 4, 5, 10, 57, 21, 3, 4, 5))

#-----------------------------------------------------------------------
# Criar um tibble por linha com os dados disponíveis em
# http://leg.ufpr.br/~walmes/data/bib1.txt.

tribble(
    ~rep , ~varied, ~bloco, ~prod,
        1,       1,      1,    20,
        1,       2,      1,    18,
        1,       3,      2,    15,
        1,       4,      2,    16
)

#-----------------------------------------------------------------------
