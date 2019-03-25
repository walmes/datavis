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
# Solução dos exercícios propostos nos slides sobre `readr`.

#-----------------------------------------------------------------------
# Carrega o pacote.

library(tidyverse)

#-----------------------------------------------------------------------
# Leitura dos dados irmpa.csv.

# url <- "~/Downloads/irmpa.csv"
url <- "http://leg.ufpr.br/~walmes/data/irmpa.csv"
tb <- read_csv2(file = url, comment = "#")
tb

glimpse(tb)

#-----------------------------------------------------------------------
# Leitura dos dados frango_comportamento.txt.

url <- "http://leg.ufpr.br/~walmes/data/frango_comportamento.txt"
tb <- read_tsv(file = url, comment = "#")
tb

glimpse(tb)

# Para importar como fator as variáveis `sexo` e `lin`.
tb <- read_tsv(file = url,
               comment = "#",
               col_types = cols(
                   .default = col_integer(),
                   filmagem = col_character(),
                   ave = col_character(),
                   sexo = col_factor(levels = NULL),
                   lin = col_factor(levels = NULL)
               ))
tb

#-----------------------------------------------------------------------
# Leitura dos dados soja.txt.

url <- "http://leg.ufpr.br/~walmes/data/soja.txt"
tb <- read_tsv(file = url,
               comment = "#",
               locale = locale(decimal_mark = ","))
tb

glimpse(tb)

#-----------------------------------------------------------------------
# Leitura dos dados compingest.txt.

url <- "http://leg.ufpr.br/~walmes/data/compingest.txt"
tb <- read_tsv(file = url, comment = "#")
tb

glimpse(tb)

#-----------------------------------------------------------------------
# Leitura dos dados carros_venda_webmotors_270314.txt.

url <- "http://leg.ufpr.br/~walmes/data/carros_venda_webmotors_270314.txt"
tb <- read_tsv(file = url, comment = "#")
tb

glimpse(tb)
View(tb)

#-----------------------------------------------------------------------
# Leitura dos dados metereologia.txt.

url <- "http://leg.ufpr.br/~walmes/data/metereologia.txt"
tb <- read_tsv(file = url, comment = "#")
tb

glimpse(tb)

#-----------------------------------------------------------------------
# Leitura dos dados euro_football_players.txt.

url <- "http://leg.ufpr.br/~walmes/data/euro_football_players.txt"

tb <- read_tsv(file = url,
               col_names = TRUE,
               quote = "",
               comment = "#")
class(tb)
str(tb, give.attr = FALSE)

#-----------------------------------------------------------------------
# Leitura dos dados saosilvestre_fwf.txt.

url <- "http://leg.ufpr.br/~walmes/data/saosilvestre_fwf.txt"

# Posições que delimitam as fronteiras dos campos.
pos <- c(0, 5, 11, 43, 47, 57, 65, 104, 126)

# Leitura com `readr`.
tb <- read_fwf(file = url,
               col_positions = fwf_widths(diff(pos)))
tb

glimpse(tb)

#-----------------------------------------------------------------------
# Leitura dos dados aval_carros_fwf.txt

# Dicionário com nome e comprimento dos campos.
url <- "http://leg.ufpr.br/~walmes/data/aval_carros_dic.txt"
dic <- read_tsv(url, col_names = FALSE)
dic

# Arquivo com as notas.
url <- "http://leg.ufpr.br/~walmes/data/aval_carros_fwf.txt"
tb <- read_fwf(file = url,
               col_positions = fwf_widths(widths = dic[[2]],
                                          col_names = dic[[1]]))
tb

# Passando o tipo de valor: c = character, i = integer.
tb <- read_fwf(file = url,
               col_types = "ciiiiiiiiiiiiiiii",
               col_positions = fwf_widths(widths = dic[[2]],
                                          col_names = dic[[1]]))
tb

glimpse(tb)

#-----------------------------------------------------------------------
