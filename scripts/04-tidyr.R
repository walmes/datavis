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
# Solução dos exercícios propostos nos slides sobre `tidyr`.

#-----------------------------------------------------------------------
# Carrega o pacote.

library(tidyverse)

#-----------------------------------------------------------------------
# Arrumar o conjunto de dados ninfas.txt.

url <- "http://leg.ufpr.br/~walmes/data/ninfas.txt"
tb <- read_tsv(file = url)
tb

glimpse(tb)

# Empilhar o número de ninfas dos terços.
tb_long <- tb %>%
    gather(key = "terço",
           value = "quantidade",
           superior, medio, inferior)
glimpse(tb_long)

# Desempilhar a quantidade de ninfas por data.
tb_wide <- tb_long %>%
    spread(key = "data",
           value = "quantidade")
glimpse(tb_wide)

#-----------------------------------------------------------------------
# Arrumar o conjunto de dados araucaria_canteiro.txt.

url <- "http://leg.ufpr.br/~walmes/data/araucaria_canteiro.txt"
tb <- read_tsv(file = url, comment = "#")
tb

# Desempilha a altura no tempo.
tb_wide <- tb[, c("tempo", "trat", "rep", "altura")] %>%
    spread(key = "tempo",
           value = "altura")
glimpse(tb_wide)

#-----------------------------------------------------------------------
# Arrumar o conjunto de dados oleos.txt.

url <- "http://leg.ufpr.br/~walmes/data/oleos.txt"
tb <- read_tsv(file = url)
tb

# Para distinguir entre registros de um mesmo tratamento (usar ao menos
# um dos dois).
# tb$rept <- 1:3      # Indicador de repetição (recliclado).
tb$ue <- 1:nrow(tb) # Indicador de unidade experimental (incremental).

# Empilha a quantidade de nematóides das avaliações.
tb_long <- tb %>%
    gather(key = "aval",
           value = "quant",
           a1:a5)
glimpse(tb_long)
tb_long

# Desempilha a quantidade de nematóides na forma de aplicação.
tb_wide <- tb_long %>%
    spread(key = "forma",
           value = "quant")
glimpse(tb_wide)

#-----------------------------------------------------------------------
# Arrumar o conjunto de dados euro_football_players.txt.

url <- "http://leg.ufpr.br/~walmes/data/euro_football_players.txt"
tb <- read_tsv(file = url,
               col_names = TRUE,
               quote = "",
               comment = "#")

# Substituir os NA por 0 (zero).
tb <- tb %>%
    replace_na(list(goal = 0, yel = 0, red = 0, ass = 0))
glimpse(tb)

#-----------------------------------------------------------------------
# Arrumar o conjunto de dados aval_carros_nota.txt.

url <- "http://leg.ufpr.br/~walmes/data/aval_carros_nota.txt"
tb <- read_tsv(file = url)
tb

# Desempilhar a nota na variável nos itens.
tb_wide <- tb %>%
    spread(key = "item",
           value = "nota")
glimpse(tb_wide)

#-----------------------------------------------------------------------
# Arrumar o conjunto de dados duster_venda_260314.txt.

url <- "http://leg.ufpr.br/~walmes/data/duster_venda_260314.txt"
tb <- read_tsv(file = url)
tb

# Separar a informação de ano e modelo.
tb <- tb %>%
    separate(col = "ano",
             into = c("ano", "mod"),
             sep = "/",
             convert = TRUE)
glimpse(tb)

# Imputar o NA no km percorrido com a média de km percorrido. Em
# categoria NA colocar `desconhecida`.
m <- mean(tb$km, na.rm = TRUE)
m

tb <- tb %>%
    replace_na(list(km = m, cat = "Desconhecida"))
glimpse(tb)

#-----------------------------------------------------------------------
# Arrumar o conjunto de dados ninfas.txt.

url <- "http://leg.ufpr.br/~walmes/data/ipea_habitacao.csv"
tb <- read_csv(file = url)
tb

# Criar a variável local com a união de município e estado.
tb <- tb %>%
    unite(col = "local",
          Município, Sigla,
          sep = " - ",
          remove = FALSE)
glimpse(tb)

#-----------------------------------------------------------------------
