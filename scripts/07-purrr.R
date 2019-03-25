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
# Solução dos exercícios propostos nos slides sobre `purrr`.

#-----------------------------------------------------------------------
# Carrega o pacote.

library(tidyverse)

#-----------------------------------------------------------------------
# Importação.

# url <- "http://leg.ufpr.br/~walmes/data/soja_grupoexperimentos.txt"
url <- "soja_grupoexperimentos.txt"
tb <- read_tsv(url, comment = "#", na = ".")
glimpse(tb)

# Conta o número de valores ausentes em cada variável.
tb %>%
    summarise_all(funs(sum(is.na(.))))

# Converte variáveis para fator.
tb <- tb %>%
    mutate_at(c("varied", "bloc", "local"), factor)

#-----------------------------------------------------------------------
# Gráficos para o rendimento de grãos.

ggplot(data = tb,
       mapping = aes(x = fct_reorder(varied, rend),
                     y = rend,
                     color = bloc)) +
    geom_point() +
    geom_line(aes(group = bloc)) +
    facet_wrap(facets = ~local) +
    labs(color = "Bloco") +
    xlab("Variedade de soja") +
    ylab(expression("Redimento de grãos" ~ (kg ~ ha^{-1}))) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5),
          legend.position = "top")

#-----------------------------------------------------------------------
# Aninhar em tabelas por local.

tb_nest <- tb %>%
    nest(-local)
tb_nest

#-----------------------------------------------------------------------
# Exemplo para fazer um quadro de anova resumido.

# Ajusta o modelo correspondente ao experimento.
tb_nest <- tb_nest %>%
    mutate(fit = map(data, lm, formula = rend ~ bloc + varied))
tb_nest
tb_nest$fit

# Obtém os quadros de ANOVA.
tb_nest <- tb_nest %>%
    mutate(anova = map(fit, anova))
tb_nest
tb_nest$anova

# Função para extrair o valor de F.
get_f_value <- function(anova_tb) {
    f_value <- anova_tb[, "F value"]
    p_value <- anova_tb[, "Pr(>F)"]
    stars <- cut(p_value,
                 breaks = c(0, 0.01, 0.05, 0.1, 1),
                 labels = c("***", "**", "*", "ns"))
    f_val <- paste(round(f_value, 4), stars, sep = " ")
    tibble(font = head(rownames(anova_tb), n = -1),
           f_val = head(f_val, n = -1))
}

# Testa a função.
get_f_value(tb_nest$anova[[1]])

# Determina os valores para cada local.
tb_nest <- tb_nest %>%
    mutate(f_values = map(anova, get_f_value))

# Para exibir o resumo dos quadros de ANOVA.
tb_nest %>%
    unnest(f_values) %>%
    spread(key = "local", value = "f_val")

#-----------------------------------------------------------------------
# Aplicar o teste de Tukey.

library(agricolae)
help(HSD.test, h = "html")

get_tukey_test <- function(model, trat) {
    out <- HSD.test(model, trat, group = TRUE, console = FALSE)
    result <- rownames_to_column(out$groups, var = trat)
    result$groups <- as.character(result$groups)
    as_tibble(result)
}

get_tukey_test(tb_nest$fit[[1]], trat = "varied")

tb_nest <- tb_nest %>%
    mutate(tukey = map(fit,
                       get_tukey_test,
                       trat = "varied"))

tb_tukey <- tb_nest %>%
    unnest(tukey)

tb_tukey %>%
    mutate(rend = round(rend, digits = 0)) %>%
    unite(col = "means", c("rend", "groups"), sep = " ") %>%
    spread(key = "local", value = "means") %>%
    print(n = Inf)

# Resultados em um gráfico.
ggplot(data = tb_result,
       mapping = aes(varied, y = rend)) +
    geom_point() +
    facet_wrap(facets = ~local, nrow = 1) +
    ylim(c(min(tb_result$rend), 5500)) +
    xlab("Variedade de soja") +
    ylab(expression("Redimento de grãos" ~ (kg ~ ha^{-1}))) +
    coord_flip() +
    geom_text(mapping = aes(label = paste(round(rend, 0), groups)),
              hjust = -0.1)

#-----------------------------------------------------------------------
# Médias ajustadas com IC.

library(lsmeans)

# Teste.
c(lsmeans(object = tb_nest$fit[[1]],
          data = tb_nest$data[[1]],
          specs = "varied"))

# Determina as médias ajustadas em todos os locais.
tb_nest <- tb_nest %>%
    mutate(lsmeans = map2(fit,
                          data,
                          function(x, y) {
                              lsm <- lsmeans(object = x,
                                             data = y,
                                             specs = "varied")
                              summary(lsm)
    }))

# Desaninha.
tb_lsmeans <- tb_nest %>%
    unnest(lsmeans)

# Resultados em um gráfico.
ggplot(data = tb_lsmeans,
       mapping = aes(varied, y = lsmean)) +
    geom_point() +
    geom_errorbar(mapping = aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0) +
    facet_wrap(facets = ~local, nrow = 1) +
    xlab("Variedade de soja") +
    ylab(expression("Redimento de grãos" ~ (kg ~ ha^{-1}))) +
    coord_flip()

# Caterpillar plot separado por local.
cater <- map(tb_nest$lsmeans,
             function(dataset) {
                 ggplot(data = dataset,
                        mapping = aes(fct_reorder(varied, lsmean),
                                      y = lsmean)) +
                     geom_point() +
                     geom_errorbar(mapping = aes(ymin = lower.CL,
                                                 ymax = upper.CL),
                                   width = 0) +
                     xlab("Variedade de soja") +
                     ylab(expression("Redimento de grãos" ~ (kg ~ ha^{-1}))) +
                     coord_flip()
             })

invoke(gridExtra::grid.arrange, cater, nrow = 1)

#-----------------------------------------------------------------------
# Manipulação de strings e fatores.

colors()

colors() %>% str_subset(pattern = "yellow")
colors() %>% str_subset(pattern = "^yellow")
colors() %>% str_subset(pattern = "yellow$")
colors() %>% str_subset(pattern = "yellow[0-9]$")
colors() %>% str_subset(pattern = "[a-z]yellow[0-9]$")
colors() %>% str_subset(pattern = "(yellow|green)")

colors() %>% str_detect(pattern = "(yellow|green)")

tb$varied %>%
    fct_unique()

tb %>% filter(str_detect(varied, "^BRS"))
tb %>% filter(str_detect(varied, "RR$"))
tb %>% filter(!str_detect(varied, "[0-9]"))
tb %>% filter(str_detect(varied, "^[[:alpha:] ]+$"))

x <- c("João Batista Nazareno",
       "Carlos Teixeira de Andrade",
       "Cecília Guimarães")
# Sobrenome.
x %>% str_replace(pattern = ".* ([[:alpha:]]+)$", replacement = "\\1")

# Nome.
x %>% str_replace(pattern = "^([[:alpha:]]+) .*", replacement = "\\1")

# Tudo exceto o sobrenome.
x %>% str_replace(pattern = "^([[:alpha:] ]+) [[:alpha:]]+$", replacement = "\\1") %>%
    gsub(pattern = "[[:lower:]]+", replacement = "")
