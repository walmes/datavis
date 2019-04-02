#---- anscombe_table ---------------------------------------------------

library(xtable)

tb <- anscombe[, c(1, 5, 2, 6, 3, 7, 4, 8)]
names(tb) <- gsub("^([xy])([1-4])$", "$\\1_\\2$", names(tb))

xtb <- xtable(tb,
              caption = "Os 4 pares de variáveis do quarteto de Ancombe.",
              digits = c(0, 0, 2, 0, 2, 0, 2, 0, 2))
print.xtable(xtb,
             caption.placement = "top",
             include.rownames = FALSE,
             sanitize.colnames.function = identity)

#---- anscombe_regression ----------------------------------------------

fits <- with(anscombe,
             expr = {
                 list(
                     "1" = lm(y1 ~  x1),
                     "2" = lm(y2 ~  x2),
                     "3" = lm(y3 ~  x3),
                     "4" = lm(y4 ~  x4))
             })

stat <- lapply(fits,
               FUN = function(m0) {
                   b <- coef(m0)
                   s <- summary(m0)
                   r2 <- s$r.squared
                   pval <- anova(m0)[1, 5]
                   data.frame("$\\hat{\\beta}_{0}$" = b[1],
                              "$\\hat{\\beta}_{1}$" = b[2],
                              "$R^2$" = r2,
                              "Valor $p$" = pval,
                              check.names = FALSE,
                              stringsAsFactors = FALSE)
               })
stat <- do.call(rbind, stat)

xtb <- xtable(stat,
              caption = "Resumo do ajuste da regressão linear simples com cada par de variáveis do quarteto de Anscombe.",
              digits = c(0, 2, 2, 2, 4))
print.xtable(xtb,
             caption.placement = "top",
             # include.rownames = FALSE,
             sanitize.colnames.function = identity)

#---- anscombe_plot ----------------------------------------------------

library(tidyverse)

cap <- "O quarteto de Anscombe em um diagrama de dispersão."

anscombe_m <- data.frame()

for (i in 1:4) {
    anscombe_m <- rbind(anscombe_m,
                        data.frame(set = i,
                                   x = anscombe[, i],
                                   y = anscombe[, i + 4]))
}

ggplot(anscombe_m, aes(x, y)) +
    geom_point(size = 3,
               fill = "orange",
               shape = 21) +
    geom_smooth(col = "gray30",
                method = lm,
                fullrange = TRUE) +
    facet_wrap(~set, ncol = 2) +
    xlab("Valores de x") +
    ylab("Valores de y") +
    theme_light()

#---- gamma_count ------------------------------------------------------

library(tidyverse)
library(gridExtra)

est_ing <- c("vegetative", "flower bud", "blossom", "fig",
             "cotton boll")
cap <- crossing(
    est = factor(est_ing, levels = est_ing),
    des = seq(0, 100, l = 5)/100,
    rept = 1:5)

cap$nc <- c(10, 9, 8, 8, 10, 11, 9, 10, 10, 10, 8, 8, 10, 8, 9, 9, 7, 7,
            8, 9, 8, 6, 6, 5, 6, 7, 8, 8, 9, 10, 9, 12, 7, 10, 9, 8, 9,
            9, 10, 8, 11, 10, 7, 8, 8, 7, 7, 7, 7, 8, 10, 9, 8, 12, 8,
            7, 5, 5, 7, 5, 6, 5, 7, 4, 7, 8, 5, 7, 6, 4, 5, 5, 4, 4, 5,
            8, 10, 7, 8, 10, 9, 6, 6, 8, 6, 9, 7, 11, 8, 9, 6, 6, 6, 6,
            7, 3, 3, 2, 4, 3, 11, 7, 9, 12, 11, 9, 13, 8, 10, 10, 9, 7,
            7, 9, 9, 8, 8, 10, 8, 10, 9, 8, 10, 8, 10)

gg_dotplot <-
    ggplot(cap, aes(x = factor(des), y = nc)) +
    facet_wrap(~est, nrow = 2) +
    geom_dotplot(binaxis = "y",
                 stackdir = "center",
                 stackratio = 1.2,
                 dotsize = 1) +
    stat_summary(aes(group = 1), geom = "line", fun.y = "mean") +
    xlab("Defoliation") +
    ylab("Number of cotton balls")

tb <- cap %>%
    group_by(est, des) %>%
    summarise(m = mean(nc), v = var(nc)) %>%
    ungroup()

r <- range(c(tb$m, tb$v))

tb_top <- tb %>%
    filter(m < 7.5) %>%
    top_n(v, n = 1)

gg_disp <-
    ggplot(tb, aes(m, v)) +
    geom_point() +
    geom_label(data = tb_top,
               mapping = aes(x = m,
                             y = v,
                             label = "n == 5"),
               parse = TRUE,
               vjust = -0.5) +
    geom_abline(intercept = 0, slope = 1, lty = 3) +
    geom_smooth(method = "lm",
                se = FALSE,
                lty = 1,
                color = "orange",
                fullrange = TRUE) +
    coord_fixed(xlim = r, ylim = r) +
    xlab("Sample mean") +
    ylab("Sample variance")

cap <- "O número de capulhos do algodão em função do nível de desfolha artificial e fase de crescimento (esq.) e a relação média-variância observada (dir.). Fonte: o autor. Visite \\href{http://www.leg.ufpr.br/~walmes/papercompanions/gammacount2014/papercomp.html}{www.leg.ufpr.br/~walmes/papercompanions}."

grid.arrange(gg_dotplot, gg_disp, nrow = 1)

#---- precipitacao_cwb -------------------------------------------------

library(tidyverse)

# Fonte: https://www.climatempo.com.br/climatologia/271/curitiba-pr

x <-
"Mês	Minima (°C)	Máxima (°C)	Precipitação (mm)
Janeiro	16°	27°	172
Fevereiro	16°	27°	158
Março	15°	26°	139
Abril	13°	23°	95
Maio	10°	21°	101
Junho	8°	20°	116
Julho	8°	19°	99
Agosto	9°	21°	73
Setembro	11°	21°	119
Outubro	13°	23°	133
Novembro	14°	25°	127
Dezembro	15°	25°	152"

tb <- read_tsv(x)
tb <- tb %>%
    mutate(min = as.integer(str_replace(`Minima (°C)`, "\\D", "")),
           max = as.integer(str_replace(`Máxima (°C)`, "\\D", "")),
           prec = `Precipitação (mm)`,
           mes = str_sub(Mês, 1, 3),
           mes = factor(mes, levels = mes))

cap <- "Precipitação e temperatura máxima e mínima mensal em Curitiba. Visite \\href{https://www.climatempo.com.br/climatologia/271/curitiba-pr}{www.climatempo.com.br/climatologia/271/curitiba-pr}."

ggplot(tb, aes(mes, prec)) +
    geom_col(color = "gray10", fill = "gray50") +
    ylab("Precipitação mensal (mm)") +
    xlab("Meses") +
    ggtitle(label = "Condições climáticas em Curitiba",
            subtitle = "Média histórica de 30 anos") +
    geom_line(data = tb,
              mapping = aes(mes, 6 * min, group = 1, color = "min")) +
    geom_line(data = tb,
              mapping = aes(mes, 6 * max, group = 1, color = "max")) +
    scale_y_continuous(
        sec.axis = sec_axis(trans = ~./6,
                            name = expression("Temperatura" ~ (degree * C)))) +
    labs(color = "Série") +
    theme(legend.position = c(0.5, 0.975),
          legend.justification = c(0.5, 1),
          legend.direction = "horizontal")

#-----------------------------------------------------------------------

# install.packages("geofacet")
# library(ggplot)
library(sf)
library(geofacet)
library(tidyverse)
ls("package:geofacet")

# br_states_grid1

# fls <- dir("../data/", pattern = "^ipeadata.*", full.names = TRUE)
#
# my_read <- function(path) {
#     y <- str_replace(path, ".*-([0-9]{4})\\.csv", "\\1")
#     tb <- read_csv2(path)
#     tb <- tb %>%
#         select(1:5)
#     names(tb) <- c("uf", "cod", "estado", "urbana", "rural")
#     tb <- tb %>%
#         add_column(ano = y)
#     return(tb)
# }
#
# tbs <- map(fls, my_read)
# tbs <- invoke(bind_rows, tbs)
#
# tbs <- tbs %>%
#     mutate(prop_urb = 100 * urbana/(urbana + rural),
#            ano = as.integer(ano))
#
# write_tsv(tbs, "../data/ipeadata_populacao_estados.txt")

tbs <- read_tsv("../data/ipeadata_populacao_estados.txt")

# aux <- tbs %>%
#     distinct(uf) %>%
#     add_column(reg = "")
# aux <- edit(aux)
# dput(aux)
aux <- tibble(
    uf = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
           "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN",
           "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
    reg = c("N", "NE", "N", "N", "NE", "NE", "CO", "SE", "CO", "NE",
            "SE", "CO", "CO", "N", "NE", "NE", "NE", "S", "SE", "NE",
            "N", "N", "S", "S", "NE", "SE", "N"))

tbs <- inner_join(tbs, aux)

gg <-
ggplot(tbs, aes(ano, prop_urb, color = reg)) +
    # facet_wrap(~uf) +
    labs(color = "Região") +
    ylim(c(0, 100)) +
    # facet_geo(~ estado, grid = "br_states_grid1", label = "name") +
    facet_geo(~ uf, grid = "br_states_grid1", label = "code") +
    geom_point() +
    geom_line(aes(group = 1)) +
    ylab("Proporção urbana da população") +
    xlab("Ano") +
    # ggtitle("Ubanização da população brasileira",
    #         "Décadas de 1950 até 2010") +
    theme(strip.text.x = element_text(margin = margin(0, 0, 0, 0, "mm"),
                                      size = 7),
          legend.position = c(1, 0),
          legend.justification = c(1, 0),
          axis.text = element_text(size = 6),
          # axis.text.x = element_text(angle = 90,
          #                            hjust = 1,
          #                            vjust = 0.5)
          )
gg

ggsave(filename = "./img/geofacet_brasil.pdf",
       plot = gg,
       width = 6,
       height = 5)

#-----------------------------------------------------------------------

# install.packages("aplpack")
library(aplpack)
library(tidyverse)

# Importação dos dados de jogadores de futebol.
url <- "http://leg.ufpr.br/~walmes/data/euro_football_players.txt"
# browseURL(url)

tb <- read_tsv(file = url,
               col_names = TRUE,
               quote = "",
               comment = "#")

# Tabela 2 só com jogadores que estiveram em campo.
# O NA no intervalo de variáveis é substituido por 0.
tb2 <- tb %>%
    filter(!is.na(apps)) %>%
    mutate_at(vars(goal:mom), replace_na, replace = 0)

# Cria posição em que joga e número total de aparições em campo.
tb2 <- tb2 %>%
    separate(apps, into = c("tit", "sub"), sep = "\\(") %>%
    mutate(tit = as.integer(tit),
           sub = as.integer(str_replace(sub, "\\D", ""))) %>%
    replace_na(list(sub = 0)) %>%
    mutate(games = tit + sub,
           position = str_replace(pos,
                                  pattern = "^([A-Z]+)\\(?.*$",
                                  replacement = "\\1"))
glimpse(tb2)

# Recodifica fator e coloca ordem lógica.
codes <- list("Goleiro" = "GK",
              "Zagueiro" = "D",
              "Meio-zaga" = "DM",
              "Meio" = "M",
              "Meio-ataque" = "AM",
              "Atacante" = "FW")

# invoke(fct_recode, c(list(tb2$position), codes))
tb2 <- tb2 %>%
    mutate(position = invoke(fct_recode, c(list(tb2$position), codes)),
           position = fct_relevel(position, names(codes)))

tb_sum <- tb2 %>%
    group_by(position) %>%
    summarise_at(c("age", "cm", "kg", "goal"),
                 funs(mean, sd, .args = list(na.rm = TRUE)))

X <- tb_sum[, -1]
X <- as.matrix(X)
rownames(X) <- tb_sum[[1]]

pdf("img/football_chernoff.pdf", width = 7, height = 5)
par(margin = c(1, 1, 1, 1))
faces(X, nrow.plot = 2, ncol.plot = 3)
dev.off()

# help(faces, h = "html")

tb_scale <- tb_sum %>%
    mutate_if(is.numeric, scale) %>%
    gather(key = "variable",
           value = "value",
           -position)

gg <-
ggplot(tb_scale, aes(x = variable,
                     y = value,
                     color = position)) +
    geom_point() +
    geom_line(aes(group = position)) +
    labs(color = "Função") +
    xlab("Estatística") +
    ylab("Valor padronizado")
gg

ggsave(filename = "img/football_paralell.pdf",
       plot = gg,
       width = 7,
       height = 3.5)

#---- visual_variables_ggplot ------------------------------------------

library(tidyverse)
library(gridExtra)

set.seed(123)
tb <- tibble(x = runif(20),
             g = sample(1:5, length(x), replace = TRUE),
             y = rnorm(length(x), mean = g + 7 * (x > 0.5)))

tm <- theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank())

g1 <-
    ggplot(tb, aes(x, y)) +
    geom_point(size = 5, pch = 1) +
    facet_wrap(~"Posição (x, y)") +
    tm
g0 <-
    ggplot(tb, aes(g, y)) +
    geom_point(size = 5, pch = 1) +
    facet_wrap(~"Posição (z, y)") +
    tm
g2 <-
    ggplot(tb, aes(x, y, size = g)) +
    geom_point(pch = 1) +
    guides(size = FALSE) +
    facet_wrap(~"Posição (x, y) + Tamanho (z)") +
    tm
g3 <-
    ggplot(tb, aes(x, y, shape = factor(g))) +
    geom_point(size = 5) +
    guides(shape = FALSE) +
    facet_wrap(~"Posição (x, y) + Forma (z)") +
    tm
g4 <-
    ggplot(tb, aes(x, y, color = g)) +
    geom_point(size = 5) +
    guides(color = FALSE) +
    facet_wrap(~"Posição (x, y) + Saturação (z)") +
    tm
g5 <-
    ggplot(tb, aes(x, y, color = factor(g))) +
    geom_point(size = 5) +
    guides(color = FALSE) +
    facet_wrap(~"Posição (x, y) + Cor (z)") +
    tm
gg <- grid.arrange(g1, g0, g2, g3, g4, g5, ncol = 2)

ggsave(filename = "img/visual-variables-ggplot2.pdf",
       plot = gg,
       width = 4,
       height = 5)

#-----------------------------------------------------------------------
