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
# Solução dos exercícios propostos nos slides sobre `dplyr`.

#-----------------------------------------------------------------------
# Carrega o pacote.

library(tidyverse)

#-----------------------------------------------------------------------
# Importação.

url <- "http://leg.ufpr.br/~walmes/data/ninfas.txt"
tb <- read_tsv(url)
glimpse(tb)

#-----------------------------------------------------------------------
# Ordenação.

tb %>% arrange(superior)
tb %>% arrange(desc(medio))
tb %>% arrange(desc(data), variedade, bloco)

#-----------------------------------------------------------------------
# Filtro.

tb %>% filter(variedade == "BRS 245 RR")

tb %>% filter(variedade == "BRS 245 RR" | variedade == "EMBRAPA 48")
tb %>% filter(variedade %in% c("BRS 245 RR", "EMBRAPA 48"))
tb %>% filter(is.element(variedade, c("BRS 245 RR", "EMBRAPA 48")))

tb %>% filter(variedade != "EMBRAPA 48")
tb %>% filter(!(variedade == "EMBRAPA 48"))

tb %>% filter(superior > 30 & inferior > 20)
tb %>% filter(superior > 30, inferior > 20)

tb %>% filter(medio >= 20 & medio <= 50)
tb %>% filter(between(medio, left = 20, right = 50))

tb %>% filter(between(data,
                      left = as.Date("2009-12-24"),
                      right = as.Date("2010-01-11")))

tb %>% filter((superior + medio + inferior) > 100)

tb %>% filter(str_detect(variedade, "BRS"))
tb %>% filter(str_detect(variedade, "CD"))
tb %>% filter(str_detect(variedade, "(CD|BRS)"))
tb %>% filter(!str_detect(variedade, "RR"))

#-----------------------------------------------------------------------
# Fatiamento.

tb %>% slice(c(34, 74, 23, 41))
tb[c(34, 74, 23, 41), ]

tb %>% slice(1:10)
tb %>% head(n = 10)
tb[1:10, ]

tb %>% slice(50:63)
tb[50:63, ]
tb %>% `[`(50:63, )

tb %>% tail(n = 10)
tb %>% slice((nrow(.) - 10):nrow(.))
tb %>% slice((n() - 10):n())

tb %>% slice(-(1:100))
tb[-(1:100), ]

tb %>% slice(c(10:15, 50:60, 100:110))
tb[c(10:15, 50:60, 100:110), ]

tb %>% slice(seq(from = 3, by = 3, to = n()))
tb[seq(from = 3, by = 3, to = nrow(tb)), ]

#-----------------------------------------------------------------------
# Amostragem aleatória.

tb %>% sample_n(size = 30)
tb %>% sample_n(size = 30, replace = TRUE)
tb %>% sample_frac(size = 0.1)

#-----------------------------------------------------------------------
# Seleção baseada nos maior/menores valores de uma variável.

tb %>% top_n(superior, n = 10)  # Maiores valores de `superior`.
tb %>% top_n(superior, n = -10) # Menores valores de `superior`.

i <- order(tb$superior, decreasing = TRUE)[1:10]
tb[i, ]

tb %>% top_n(superior, n = -10) # Menores valores de `superior`.

#-----------------------------------------------------------------------
# Seleção de variáveis.

tb %>% select(superior, medio, inferior)
tb %>% select(c("superior", "medio", "inferior"))
tb[, c("superior", "medio", "inferior")]
tb %>% select(4:6)
tb[, 4:6]

tb %>% select(-bloco)
tb %>% select(-c("bloco"))
tb %>% select(-3)

tb %>% select(data:bloco, inferior, medio, superior)
tb %>% select(1:3, 6, 5, 4)

tb %>% select(ends_with("rior"))
tb %>% select(contains("a"))
tb %>% select(contains("e"))

tb %>% select_if(is.numeric)
tb %>% select_if(negate(is.numeric))

my_test <- function(x) {
    is.numeric(x) && var(x) > 10
}
tb %>% select_if(my_test)

#-----------------------------------------------------------------------
# Criação/modificação de variáveis.

tb %>% mutate(total = superior + medio + inferior,
              diff = superior - inferior)

tb %>% mutate(s_sup = sqrt(superior),
              s_med = sqrt(medio),
              s_inf = inferior^0.5)
tb %>% mutate_at(vars(superior:inferior), sqrt)
tb %>% mutate_at(4:6, sqrt)
tb %>% mutate_if(is.numeric, sqrt)

tb %>% mutate_at(c("bloco", "variedade"), factor)

#-----------------------------------------------------------------------
# Renomear variáveis.

tb %>% rename(tratamento = "variedade")

tb %>% rename(sup = "superior",
              med = "medio",
              inf = "inferior")

tb %>% rename_all(toupper)
tb %>% rename_all(abbreviate)
tb %>% rename_all(function(x) str_sub(x, 1, 3))

#-----------------------------------------------------------------------
# Resumação.

tb %>% summarise(sup_tot = sum(superior))

tb %>% summarise(sup_tot = sum(superior),
                 med_tot = sum(medio),
                 inf_tot = sum(inferior))
tb %>% summarise_at(4:6, sum)
tb %>% summarise_at(vars(superior:inferior), sum)
tb %>% summarise_if(is.numeric, sum)

tb %>% summarise_at(vars(ends_with("rior")), sum)
tb %>% summarise_at(vars(matches("^(s|m|i)")), sum)

tb %>% summarise(sup_med = mean(superior),
                 med_med = mean(medio),
                 inf_med = mean(inferior),
                 sup_sd = sd(superior),
                 med_sd = sd(medio),
                 inf_sd = sd(inferior))
tb %>% summarise_at(4:6, funs(mean, sd))

cv <- function(x) 100 * sd(x)/mean(x)
tb %>% summarise_at(4:6, cv)

tb %>% summarise(sup_med = cor(superior, medio),
                 sup_inf = cor(superior, inferior),
                 med_inf = cor(medio, inferior))

my_cor <- function(X) {
    corr <- cor(X)
    nms <- rownames(corr)
    nms <- combn(nms, 2)
    nms <- paste(nms[1, ], nms[2, ], sep = ":")
    corr <- c(corr[lower.tri(corr)])
    return(tibble(pair = nms, correlation = corr))
}

tb %>%
    do(my_cor(cbind(.[, 4:6])))

tb %>%
    summarise(cors = list(my_cor(cbind(superior, medio, inferior)))) %>%
    unnest()

#-----------------------------------------------------------------------
# Agregações.

tb %>% count(variedade)
tb %>% count(data)
tb %>% count(data, variedade)

tb %>%
    count(data, variedade) %>%
    spread(key = data, value = "n")

tb %>%
    group_by(data) %>%
    summarise(sup = sum(superior))

tb %>%
    group_by(data) %>%
    summarise(sup = sum(superior + medio + inferior))

tb %>%
    group_by(variedade) %>%
    summarise(sup = sum(superior + medio + inferior)) %>%
    arrange(sup)

tb1 <- tb %>%
    group_by(data, variedade) %>%
    summarise(total = sum(superior + medio + inferior))
tb1

tb1 %>%
    group_by(data) %>%
    top_n(total, n = 2)

tb1 %>%
    group_by(data) %>%
    top_n(total, n = -1)

tb1 %>%
    group_by(variedade) %>%
    top_n(total, n = -1)

tb1 %>%
    arrange(total) %>%
    group_by(variedade) %>%
    slice(c(1, n()))

tb1 %>%
    arrange(total) %>%
    group_by(variedade) %>%
    summarise(menor = first(total),
              maior = last(total))

tb1 %>%
    group_by(variedade) %>%
    summarise(menor = min(total),
              maior = max(total))

#-----------------------------------------------------------------------
