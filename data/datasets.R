#-----------------------------------------------------------------------

library(tidyverse)

options(readr.num_columns)
getOption("readr.num_columns")

names(options()) %>%
    str_subset("readr")

# options(readr.show_progress = FALSE,
#         readr.num_columns = 0)

#-----------------------------------------------------------------------
# Prepara arquivo de dados dos votos.

# Lê todos os arquivos.
f <- dir(pattern = "votos-.*\\.csv")
tb <- map(f,
          function(x) {
              tb <- read_csv2(x, col_types = cols())
              tb$X10 <- NULL
              names(tb) <- c("estado",
                             "codigo",
                             "municipio",
                             "branco_1_turno",
                             "branco_2_turno",
                             "nulo_1_turno",
                             "nulo_2_turno",
                             "valido_2_turno",
                             "valido_1_turno")
              tb$ano <- as.integer(str_replace_all(x, "\\D", ""))
              return(tb)
          })

# Empilha.
tb <- invoke(bind_rows, tb)
tb

# Troca a ordem.
tb <- tb %>%
    select(ano, everything())
tb

# Escreve em disco.
write_tsv(tb, path = "eleicoes_presidente.txt")

# tb %>%
#     group_by(ano, uf) %>%
#     summarise_at(vars(bran1:vali2), sum, na.rm = TRUE)

#-----------------------------------------------------------------------
# População residente por sexo.

# Lê todos os arquivos.
f <- dir(pattern = "ipeadata-.*\\.csv")
tb <- map(f,
          function(x) {
              # tb <- read_csv2(x)
              tb <- read_csv2(x, col_types = cols())
              tb$X8 <- NULL
              names(tb) <- c("uf",
                             "codigo",
                             "estado",
                             "urbana_mulher",
                             "rural_mulher",
                             "urbana_homem",
                             "rural_homem")
              tb$ano <- as.integer(str_replace_all(x, "\\D", ""))
              return(tb)
          })

# Empilha.
tb <- invoke(bind_rows, tb)
tb

# Troca a ordem.
tb <- tb %>%
    select(ano, everything())
tb

# Escreve em disco.
write_tsv(tb, path = "populacao_residente.txt")

# tb <- tb %>%
#     gather(key = "var", value = "habit", contains("_")) %>%
#     separate("var", c("zona", "sexo"))
#
# tb <- tb %>%
#     group_by(ano, estado) %>%
#     mutate(perc = habit/sum(habit, na.rm = TRUE)) %>%
#     ungroup()
#
# ggplot(tb, aes(x = ano, y = perc, color = sexo, linetype = zona)) +
#     facet_wrap(facets = ~estado) +
#     # facet_wrap(facets = ~estado, scales = "free") +
#     geom_line()

#-----------------------------------------------------------------------
