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
# Solução dos exercícios propostos nos slides sobre `stringr`.

#-----------------------------------------------------------------------
# Carrega o pacote.

library(tidyverse)

#-----------------------------------------------------------------------
# Retornar o conjunto que bate.

x <- colors()
length(x)

x %>% str_subset(pattern = "yellow")
x %>% str_subset(pattern = "^yellow")
x %>% str_subset(pattern = "yellow$")
x %>% str_subset(pattern = "yellow[0-9]$")
x %>% str_subset(pattern = "[a-z]yellow[0-9]$")
x %>% str_subset(pattern = "(yellow|green)")

#-----------------------------------------------------------------------
# Retorna vetor lógico onde TRUE indica que o padrão ocorre.

x %>% str_detect(pattern = "yellow") %>% table()
x %>% str_detect(pattern = "^yellow") %>% table()
x %>% str_detect(pattern = "\\d") %>% table()
x %>% str_detect(pattern = "[0-9]") %>% table()

#-----------------------------------------------------------------------
# Extração de padrões.

x <- c("João Batista Nazareno",
       "Carlos Teixeira de Andrade",
       "Cecília Guimarães")

# A primeira letra.
x %>% str_replace(pattern = "^(.).*", replacement = "\\1")

# A primeira palavra.
x %>% str_replace(pattern = "^([A-Za-z]+) .*", replacement = "\\1")
x %>% str_replace(pattern = "^([[:alpha:]]+) .*", replacement = "\\1")

# OBS: [[:alpha:]] é melhor que [A-Za-z] pois contém: á, é, etc.

# Apenas o sobrenome.
x %>% str_replace(pattern = ".* ([[:alpha:]]+)$", replacement = "\\1")

# Apenas primeiro nome com sobrenome.
x %>% str_replace(pattern = "^([[:alpha:]]+).* ([[:alpha:]]+)$",
                  replacement = "\\1 \\2")

# Tudo exceto o último nome, retornar só a primeira letra.
x %>%
    str_replace(pattern = "^([[:alpha:] ]+) [[:alpha:]]+$",
                replacement = "\\1") %>%
    str_replace_all(pattern = "[[:lower:]]+ *",
                    replacement = "") %>%
    str_replace_all(pattern = "(.)", replacement = "\\1.")

#-----------------------------------------------------------------------
