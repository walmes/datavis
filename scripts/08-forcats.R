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
# Solução dos exercícios propostos nos slides sobre `forcats`.

#-----------------------------------------------------------------------
# Carrega o pacote.

library(tidyverse)

#-----------------------------------------------------------------------
# Criação de fatores.

# Experimento para estudar um fator categórico de 5 níveis com 4
# repetições em blocos casualizados.
set.seed(1234)
da <- crossing(bloco = 1:4, trat = letters[1:5])
da <- da %>%
    mutate(cat1 = sample(1:3, size = n(), replace = TRUE),
           cat2 = c(sample(1:5, size = n() - 2, replace = TRUE), NA, NA),
           y = rpois(n(), lambda = 30))

glimpse(da)

da %>% xtabs(formula = ~bloco + trat)
da %>% xtabs(formula = ~trat + cat1)

# Conversão para fator.
da <- da %>%
    mutate(trat = factor(trat),
           bloco = factor(bloco, labels = as.roman(unique(bloco))),
           cat1 = factor(cat1),
           cat2 = factor(cat2, levels = c(1:7)))
glimpse(da)

#-----------------------------------------------------------------------
# Contar níveis, exibir rótulos e determinar a frequência.

# Determinar o número de níveis do fator.
da$trat %>% nlevels()
da$bloco %>% nlevels()
da$cat1 %>% nlevels()
da$cat2 %>% nlevels()

# Conhecer rótulo dos níveis do fator.
da$bloco %>% fct_unique()
da$trat %>% fct_unique() # Retorna como fator.
da$trat %>% levels()     # Retorna como character.

# Conta a ocorrência de cada nível.
da %>% count(cat2)       # Não conta os níveis vazios.
da$cat2 %>% table()      # Não conta os NAs.
da$cat2 %>% fct_count()
da$cat2 %>% fct_count(sort = TRUE)

#-----------------------------------------------------------------------
# Mudar a disposição/ordem dos níveis de um fator.

# Coloca os níveis em ordem inversa.
da$trat %>% fct_rev()

# Aleatoriza a posição dos níveis.
da$trat %>% fct_shuffle()
da$bloco %>% fct_shuffle()

# Coloca a ordem conforme a frequência de cada nível.
da$cat1 %>% fct_infreq()
da$cat2 %>% fct_infreq() %>% fct_rev()

# Ordena os níveis conforme estatísticas de outra variável.
da %>%
    group_by(cat1) %>%
    summarise_at("y", funs(mean, median, max, min, length))

# Ordena por alguma estatística de grupo da variável `y`.
da$cat1 %>% fct_reorder(da$y, .fun = "median")
da$cat1 %>% fct_reorder(da$y, .fun = "max")
da$cat1 %>% fct_reorder(da$y, .fun = "length")

# Muda a disposição manualmente.
da$trat %>% fct_relevel("b", "e")
da$trat %>% fct_relevel("b", "e", after = 2)
da$trat %>% fct_relevel(c("b", "e"), after = 2)

#-----------------------------------------------------------------------
# Aumentar/reduzir o número de níveis.

# Abandona níveis vazios, ou seja, que não ocorreram.
da$cat2 %>% fct_count()
da$cat2 %>% fct_drop()
da$cat2 %>% fct_drop(only = "6")

# Aglutina níveis para criar uma nova categoria.
da$cat2 %>%
    fct_other(drop = c("1", "4"), other_level = "outro") %>%
    fct_count()
da$cat2 %>%
    fct_other(keep = c("1", "4"), other_level = "outro") %>%
    fct_count()

# Número e proporção de cada nível.
da$cat2 %>%
    fct_count() %>%
    mutate(p = n/nrow(da))

# Aglutina níveis para preservar os `n` mais frequentes.
# OBS: em caso de empate em frequência, pode ter mais níveis que `n`.
# Valor negativo de `n` inverte a lógica.
da$cat2 %>%
    fct_lump(n = 3, other_level = "outro") %>%
    fct_count()

# Preserva os níveis com frequência relativa a partir de `prop`.
# OBS: fique atento para o caso de empate em frequência relativa.
# Valor negativo de `prop` inverte a lógica.
da$cat2 %>%
    fct_lump(prop = 0.18, other_level = "outro") %>%
    fct_count() %>%
    mutate(p = n/nrow(da))

# Agrupa níveis.
da$cat1 %>% fct_collapse(u = c("1", "3"))
da$cat2 %>% fct_collapse("123" = c("1", "2", "3"), "45" = c("4", "5"))

# Adiciona níveis ao fator (mas não entradas no vetor).
da$trat %>% fct_expand("u", "w")
da$trat %>% fct_expand(c("z", "y"))

#-----------------------------------------------------------------------
# Editar os rótulos do fator.

# Aplica uma função aos rótulos.
da$trat %>% fct_relabel(toupper)
da$bloco %>% fct_relabel(tolower)
da$trat %>% fct_relabel(function(x) str_c(x, "_new"))
da$trat %>% fct_relabel(~str_c(., "_new"))
da$cat1 %>% fct_relabel(~as.character(as.roman(as.integer(.))))

# Recodifica manualmente os níveis no fator.
da$trat %>% fct_recode("Azul" = "a", "Branco" = "b")

# Lista para recodificação.
rcd <- list("Azul" = "a",
            "Branco" = "b",
            "Cinza" = "c",
            "Dourado" = "d",
            "Esmeralda" = "e")

# Aplicação chamando a função `fct_recode()` passando a lista de
# argumentos.
invoke(fct_recode, c(list(da$trat), rcd))
do.call(fct_recode, args = c(list(da$trat), rcd))

# Anonimatiza os níveis deixando rótulos inteiros começando em 1.
da$trat %>% fct_anon()
da$bloco %>% fct_anon()

# Cria um rótulo para níveis que são valores ausentes.
da$cat2 %>% fct_explicit_na(na_level = "desconhecido")

#-----------------------------------------------------------------------
