#---- read_futbol ------------------------------------------------------

library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(tidyverse)
library(gridExtra)
library(ggjoy)
library(waffle)
library(ggmosaic)
library(GGally)
library(corrplot)
library(wordcloud)
library(ggridges)
library(ggradar) # ricardo-bion/ggradar
library(fmsb)

path <- "../data/euro_football_players.txt"

if (!file.exists(path)) {
    url <- "http://leg.ufpr.br/~walmes/data/euro_football_players.txt"
    download.file(url, destfile = path)
}

# Importação.
tb <- read_tsv(file = path,
               col_names = TRUE,
               col_types = cols(),
               quote = "",
               comment = "#")
attr(tb, "spec") <- NULL
# class(tb)
# glimpse(tb)

# Remove os sem aparição e substitui os zeros.
tb2 <- tb %>%
    filter(!is.na(apps)) %>%
    replace_na(list(goal = 0,
                    red = 0,
                    yel = 0,
                    spg = 0))
# glimpse(tb2)

# Criar a posição em que joga e número de aparições.
tb2 <- tb2 %>%
    # distinct(apps) %>%
    separate(apps, into = c("tit", "res"), sep = "\\(") %>%
    mutate(tit = as.integer(tit),
           res = str_replace(res, "\\D", "") %>% as.integer()) %>%
    replace_na(list(res = 0)) %>%
    mutate(bmi = kg/(cm/100)^2,
           games = tit + res,
           position = str_replace(pos,
                                  pattern = "^([A-Z]+)\\(?.*$",
                                  replacement = "\\1"),
           position = factor(position,
                             levels = c("GK", "D", "DM", "M", "AM", "FW")))
# glimpse(tb2)
# str(tb2, vec.len = 2)
glimpse(tb2, width = 60)

#---- height_stem ------------------------------------------------------

x <- tb2 %>%
    filter(country == "England") %>%
    pull(cm)

cat("Número de jogadores: ", length(x), "\n", sep = "")
stem(x)

#---- height_dotplot ---------------------------------------------------

# stripchart(x, method = "stack", pch = 19, offset = 0.5,
#            ylim = c(1, 2),
#            xlab = "Altura dos jogadores ingleses (cm)")

ggplot(data = data.frame(x = x),
       mapping = aes(x = x)) +
    geom_dotplot(binwidth = 1, stackratio = 1.1) +
    xlab("Altura dos jogadores ingleses (cm)") +
    scale_y_continuous(name = NULL, breaks = NULL)

cap <- "Distribuição da altura dos jogadores ingleses."

#---- height_histogram -------------------------------------------------

gg <- ggplot(data = data.frame(x = x),
             mapping = aes(x = x)) +
    geom_rug() +
    xlab("Altura dos jogadores ingleses (cm)") +
    ylab("Frequência absoluta")

grid.arrange(gg + geom_histogram(binwidth = 1, color = "black"),
             gg + geom_histogram(binwidth = 3, color = "black"),
             nrow = 1)

cap <- "Distribuição da altura dos jogadores ingleses."

#---- height_density ---------------------------------------------------

gg <- ggplot(data = data.frame(x = x),
             mapping = aes(x = x)) +
    geom_rug() +
    xlab("Altura dos jogadores ingleses (cm)") +
    ylab("Densidade")

grid.arrange(gg + geom_density(bw = 1, fill = "orange", alpha = 0.2),
             gg + geom_density(bw = 3, fill = "tomato", alpha = 0.2),
             nrow = 1)

cap <- "Distribuição da altura dos jogadores ingleses."

#---- height_ecdf ------------------------------------------------------

gg0 <-
    ggplot(data = data.frame(x = x),
           mapping = aes(x = x)) +
    stat_ecdf() +
    xlab("Altura dos jogadores ingleses (cm)") +
    ylab("Frequência relativa acumulada")

m <- median(x)
mm <- min(x)

kx <- c(170, 180)
ky <- cut(x, c(-Inf, kx, Inf)) %>%
    table() %>%
    prop.table() %>%
    cumsum() %>%
    head(n = 2)

gg1 <- gg0 +
    geom_segment(aes(x = m, y = 0, xend = m, yend = 0.5), color = "red") +
    geom_segment(aes(x = m, y = 0.5, xend = mm, yend = 0.5), color = "red") +
    geom_segment(aes(x = kx[1], y = 0, xend = kx[1], yend = ky[1]), color = "blue") +
    geom_segment(aes(x = kx[1], y = ky[1], xend = mm, yend = ky[1]), color = "blue") +
    geom_segment(aes(x = kx[2], y = 0, xend = kx[2], yend = ky[2]), color = "blue") +
    geom_segment(aes(x = kx[2], y = ky[2], xend = mm, yend = ky[2]), color = "blue")

grid.arrange(gg0, gg1, nrow = 1)

cap <- "Distribuição da altura dos jogadores ingleses."

#---- height_boxplot ---------------------------------------------------

x <- tb2 %>%
    filter(position == "GK") %>%
    pull(cm)

gg0 <- ggplot(data = data.frame(x = x),
       mapping = aes(x = 1, group = 1, y = x)) +
    geom_boxplot(fill = "orange", alpha = 0.5) +
    ylab("Altura dos jogadores ingleses (cm)") +
    scale_x_continuous(name = NULL, breaks = NULL) +
    geom_rug(sides = "b") +
    coord_flip()

gg1 <- ggplot(data = data.frame(x = x),
       mapping = aes(x = x)) +
    geom_density(fill = "orange", alpha = 0.5) +
    geom_rug() +
    scale_x_continuous(name = NULL, breaks = NULL) +
    scale_y_continuous(name = NULL, breaks = NULL) +
    geom_vline(xintercept = fivenum(x), linetype = 3)

grid.arrange(gg1, gg0, ncol = 1)

cap <- "Distribuição da altura dos goleiros."

#---- height_violin ---------------------------------------------------

gg1 <- ggplot(data = data.frame(x = x),
       mapping = aes(x = 1, group = 1, y = x)) +
    geom_violin(fill = "orange", alpha = 0.5) +
    ylab("Altura dos goleiros (cm)") +
    scale_x_continuous(name = NULL, breaks = NULL) +
    geom_rug(sides = "l") +
    geom_vline(xintercept = 1, linetype = 2)

gg0 <- ggplot(data = data.frame(x = x),
       mapping = aes(x = 1, group = 1, y = x)) +
    geom_boxplot(fill = "orange", alpha = 0.5) +
    ylab("Altura dos goleiros (cm)") +
    scale_x_continuous(name = NULL, breaks = NULL) +
    geom_rug(sides = "l")

grid.arrange(gg1, gg0, nrow = 1)

cap <- "Distribuição da altura dos goleiros."

#---- height_joy -------------------------------------------------------

gg0 <- ggplot(tb2,
       aes(x = cm, y = position)) +
    geom_density_ridges(rel_min_height = 0.005, scale = 1.2) +
    scale_y_discrete(expand = c(0.01, 0)) +
    scale_x_continuous(expand = c(0.01, 0)) +
    xlab("Altura dos jogadores (cm)") +
    ylab("Função em campo")

gg1 <- ggplot(tb2,
       aes(x = cm)) +
    facet_grid(facets = position ~ .,
               as.table = FALSE) +
    geom_density(fill = "purple", alpha = 0.5) +
    xlab("Altura dos jogadores (cm)") +
    ylab("Densidade")

grid.arrange(gg0, gg1, nrow = 1)

cap <- "Distribuição da altura dos jogadores por função em campo."

#---- height_compare ---------------------------------------------------

gg <- ggplot(tb2,
             aes(x = position, y = cm)) +
    ylab("Altura (cm)") +
    xlab("Função em campo")

grid.arrange(gg + geom_boxplot(fill = "yellow", alpha = 0.5),
             gg + geom_violin(fill = "cyan", alpha = 0.5),
             ncol = 1)

cap <- "Distribuição da altura dos jogadores por função em campo."

#---- position_bar --------------------------------------------------

gg0 <- ggplot(tb2,
             aes(x = position)) +
    geom_bar(fill = "tomato", color = "black") +
    ylab("Número de jogadores") +
    xlab("Função em campo")

gg1 <- ggplot(tb2,
              aes(x = red)) +
    geom_bar(fill = "navyblue", color = "black") +
    ylab("Número de jogadores") +
    xlab("Número de cartões vermelhos")

grid.arrange(gg0, gg1, nrow = 1)

cap <-
    "Distribuição dos jogadores quanto a função em campo e número de cartões vermelhos."

#---- position_pie -----------------------------------------------------

gg0 <- ggplot(tb2,
              aes(x = 0, fill = position)) +
    geom_bar(color = "black") +
    coord_polar(theta = "y") +
    ylab(NULL) +
    xlab(NULL) +
    scale_x_continuous(name = NULL, breaks = NULL) +
    labs(fill = "Função")

gg1 <- ggplot(tb2,
              aes(x = 0, fill = factor(red))) +
    geom_bar(color = "black") +
    coord_polar(theta = "y") +
    ylab(NULL) +
    xlab(NULL) +
    scale_x_continuous(name = NULL, breaks = NULL) +
    labs(fill = "Função")

grid.arrange(gg0, gg1, nrow = 1)

cap <- "Distribuição dos jogadores quanto a função em campo e número de cartões vermelhos."

#---- position_donut ---------------------------------------------------

gg0 <- ggplot(tb2,
              aes(x = 0, fill = position)) +
    geom_bar(color = "black", width = 1) +
    coord_polar(theta = "y") +
    ylab(NULL) +
    xlab(NULL) +
    scale_x_continuous(name = NULL,
                       breaks = NULL,
                       limits = c(-2, 0.5)) +
    scale_y_continuous(breaks = NULL) +
    labs(fill = "Função")

gg1 <- ggplot(tb2,
              aes(x = 0, fill = position)) +
    geom_bar(color = "black") +
    coord_polar(theta = "y") +
    ylab(NULL) +
    xlab(NULL) +
    scale_x_continuous(name = NULL, breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    labs(fill = "Função")

grid.arrange(gg0, gg1, nrow = 1)

cap <- "Distribuição dos jogadores quanto a função em campo."

#---- redcard_waffle ---------------------------------------------------

tbf <- table(tb2$red)
r <- 25
g <- sprintf("Cada unidade é um jogador (grid %d x %d)",
             r,
             ceiling(sum(tbf)/r))

waffle(tbf, rows = r, size = 1, xlab = g)

cap <- "Distribuição dos jogadores quanto ao número de cartões vermelhos."

#---- bmi_qq -----------------------------------------------------------

gg0 <- ggplot(tb2, aes(sample = age)) +
    geom_qq(pch = 1) +
    geom_qq_line(linetype = 3) +
    ylab("Idade dos jogadores (anos)") +
    xlab("Quantis teóricos da\ndistribuição normal padrão")

gg1 <- ggplot(tb2, aes(sample = bmi)) +
    geom_qq(pch = 1) +
    geom_qq_line(linetype = 3) +
    ylab("Índice de massa corporal") +
    xlab("Quantis teóricos da\ndistribuição normal padrão")

grid.arrange(gg0, gg1, nrow = 1)

cap <- "Distribuição da idade e índice de massa corportal dos jogadores."

#---- yel_stacked_bar --------------------------------------------------

tb2 <- tb2 %>%
    mutate(yelg = cut(yel,
                      # c(0, 0.5, 1, 2, 4, Inf),
                      # c("0", "1", "2", "3 a 4", "5+"),
                      c(0, 0.5, 2, Inf),
                      c("0", "1 a 2", "2+"),
                      include.lowest = TRUE))

gg0 <- ggplot(tb2, aes(x = position, fill = fct_rev(yelg))) +
    geom_bar(color = "black") +
    ylab("Número de jogadores") +
    xlab("Função em campo") +
    labs(fill = "Cartões\namarelos") +
    scale_fill_brewer(palette = 7, direction = -1)
gg0

cap <- "Quantidade de cartões amarelos e a relação com a função em campo."

#---- yel_grouped_bar --------------------------------------------------

gg0 <- ggplot(tb2, aes(x = position, fill = yelg)) +
    geom_bar(color = "black", position = "dodge") +
    ylab("Número de jogadores") +
    xlab("Função em campo") +
    labs(fill = "Cartões\namarelos") +
    scale_fill_brewer(palette = 7, direction = 1)
gg0

cap <- "Quantidade de cartões amarelos e a relação com a função em campo."

#---- yel_filled_bar ---------------------------------------------------

gg0 <- ggplot(tb2, aes(x = position, fill = fct_rev(yelg))) +
    geom_bar(color = "black", position = "fill") +
    ylab("Proporção de jogadores") +
    xlab("Função em campo") +
    labs(fill = "Cartões\namarelos") +
    scale_fill_brewer(palette = 7, direction = -1)
gg0

cap <- "Quantidade de cartões amarelos e a relação com a função em campo."

#---- yel_mosaic -------------------------------------------------------

ggplot(data = tb2) +
    geom_mosaic(aes(x = product(yelg, position),
                    fill = yelg,
                    na.rm = TRUE),
                alpha = 1,
                size = 0.6,
                color = "black") +
    ylab("Número de cartões") +
    xlab("Função em campo") +
    labs(fill = "Cartões\namarelos") +
    theme(legend.key = element_rect(colour = "black", size = 1)) +
    scale_fill_brewer(palette = 7, direction = 1)

cap <- "Quantidade de cartões amarelos e a relação com a função em campo."

#---- cm_x_kg ----------------------------------------------------------

tb2_gk <- tb2 %>%
    filter(position == "GK")

gg0 <- ggplot(data = tb2_gk,
              aes(x = kg, y = cm)) +
    ylab("Altura do goleiro (cm)") +
    xlab("Peso do goleiro (kg)")

grid.arrange(gg0 + geom_point(pch = 1, size = 2),
             gg0 + geom_jitter(pch = 1, size = 2),
             nrow = 1)

cap <- "Relação altura e peso dos goleiros. O gráfico da direita foi adicionada pertubação para evitar sobreposição dos pontos."

#---- cm_x_kg_x_age ----------------------------------------------------

gg0 <- ggplot(data = tb2_gk,
              aes(x = kg, y = cm)) +
    ylab("Altura do goleiro (cm)") +
    xlab("Peso do goleiro (kg)")

gg1 <- gg0 +
    geom_point(aes(fill = age), pch = 21, size = 3) +
    scale_fill_distiller(palette = "Spectral", direction = -1) +
    labs(color = "Idade") +
    theme_light()

grid.arrange(gg0 +
             geom_point(aes(size = age), pch = 1) +
             labs(size = "Idade"),
             gg1,
             nrow = 1)

cap <- "Relação altura, peso e idade dos goleiros."

#---- ggpairs ----------------------------------------------------------

ggpairs(tb2_gk[, c("cm", "kg", "age")])
cap <- "Matriz de diagramas de dispersão."

#---- correlogram ------------------------------------------------------

M <- cor(tb2_gk[, c("cm", "kg", "age", "bmi", "aw")], use = "complete")

par(mfrow = c(1, 2))
corrplot(M, method = "square", order = "AOE", addCoef.col = "grey")
corrplot(M, method = "ellipse", order = "AOE", addCoef.col = "grey")
layout(1)

cap <- "Correlograma das variáveis para os goleiros."

#---- heatmap ----------------------------------------------------------

scale01 <- function(x) {
    (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE))
}

tb2_gk2 <- tb2_gk %>%
    top_n(games, n = 20) %>%
    mutate_at(vars(cm, kg, age, bmi, aw), scale01) %>%
    select(name, cm, kg, age, bmi, aw) %>%
    drop_na() %>%
    gather(key = "var", value = "value", -name)

ggplot(tb2_gk2, aes(var, name)) +
    geom_tile(aes(fill = value), colour = "black") +
    ylab("Goleiro") +
    xlab("Variável") +
    scale_fill_gradient(name = "Valor\nrelativizado",
                        low = "white",
                        high = "orange")

cap <- "Mapa de calor das variáveis para os goleiros."

#---- parallel ---------------------------------------------------------

tb2_gk2 <- tb2_gk2 %>%
    mutate(var = fct_relevel(var, "bmi", after = Inf))

lb <- tb2_gk2 %>%
    filter(var == "bmi")

ggplot(tb2_gk2, aes(var, value, color = name)) +
    geom_point() +
    geom_text(data = lb,
              aes(x = var, y = value, label = name),
              hjust = 0,
              nudge_x = 0.1,
              color = "black",
              check_overlap = FALSE) +
    geom_line(aes(group = name)) +
    expand_limits(x = c(1, 7)) +
    xlab("Variável") +
    ylab("Valor padronizado") +
    guides(colour = FALSE)

cap <- "Gráfico de eixos paralelos das variáveis para os goleiros."

#---- spider -----------------------------------------------------------

u <- tb2_gk2 %>%
    spread(key = "var", value = "value")
u <- bind_rows(u %>% summarise_if(is.numeric, min),
          u %>% summarise_if(is.numeric, max)) %>%
    add_column(name = c("min", "max")) %>%
    bind_rows(u) %>%
    select(name, everything())

oldpar <- par()
par(mar = c(0, 0, 0, 0))
radarchart(u[, -1])
par(oldpar)

# ggradar(u[-(1:2), ])

cap <- "Gráfico de radar das variáveis para os goleiros."

#---- cleveland --------------------------------------------------------

set.seed(123)
s <- sample(unique(tb2$team), size = 15)

u <- tb2 %>%
    filter(team %in% s) %>%
    mutate(goleiro = ifelse(position == "GK", "goleiro", "jogador")) %>%
    group_by(team, goleiro) %>%
    summarise(cm = mean(cm, na.rm = TRUE))

ggplot(u, aes(cm, team, color = goleiro)) +
    geom_point() +
    labs(color = "Função", x = "Altura (cm)", y = "Equipe")

cap <- "Altura média por equipe para goleiros e jogadores da linha."

#---- boxplots ---------------------------------------------------------
# Tufte boxplot e tema Tufte para o ggplot2.
# https://stackoverflow.com/questions/6973394/functions-available-for-tufte-boxplots-in-r
# https://www.datanovia.com/en/blog/ggplot-themes-gallery/

gg <- ggplot(tb2, aes(x = position)) +
    xlab("Função em campo")

gg1 <- gg +
    geom_boxplot(aes(y = cm), fill = "tomato") +
    ylab("Altura (cm)")

gg2 <- gg +
    geom_boxplot(aes(y = kg), fill = "springgreen") +
    ylab("Peso (kg)")

gg3 <- gg +
    geom_boxplot(aes(y = age), fill = "pink") +
    ylab("Idade (anos)")

grid.arrange(gg1, gg2, gg3, nrow = 1)

cap <- "Diagramas de caixa para altura, peso e idade dos jogadores por função."

#---- hexbin -----------------------------------------------------------

gg <- ggplot(tb2, aes(x = cm, y = kg)) +
    labs(fill = "Frequência", x = "Altura (cm)", y = "Peso (kg)") +
    scale_fill_distiller(palette = "RdBu", direction = -1) +
    theme(legend.position = c(0.05, 0.95),
          legend.justification = c(0, 1))

grid.arrange(gg + geom_bin2d(),
             gg + geom_hex(),
             nrow = 1)

cap <- "Densidade de jogadores conforme a classificação por altura e peso."

#---- ordered_bar ------------------------------------------------------

tb_gols <- tb2 %>%
    top_n(goal, n = 20)

ggplot(tb_gols, aes(reorder(name, goal), goal)) +
    geom_col() +
    xlab("Jogador") +
    ylab("Gols marcados") +
    coord_flip()

cap <- "Jogadores classificados pelo número de gols feito."

#---- lillipop ---------------------------------------------------------

ggplot(tb_gols, aes(reorder(name, goal), goal)) +
    geom_point() +
    geom_segment(aes(xend = name, y = 0, yend = goal)) +
    xlab("Jogador") +
    ylab("Gols marcados") +
    coord_flip()

cap <- "Jogadores classificados pelo número de gols feito."

#---- wordcloud --------------------------------------------------------

tb_freq <- tb2 %>%
    count(country)

par(mar = c(0, 0, 0, 0), mfrow = c(1, 2))
wordcloud(words = tb_freq[[1]],
          freq = tb_freq[[2]],
          random.order = FALSE,
          col = "navyblue",
          min.freq = 1,
          rot.per = 0)
wordcloud(words = tb2$name,
          freq = tb2$goal^2,
          random.order = FALSE,
          min.freq = 1,
          col = "green4",
          max.words = 50,
          rot.per = 0)

cap <- "Países conforme o número de jogadores de cada nacionalidade e jogadores conforme o número de gols."

#-----------------------------------------------------------------------

# library(HistData)
# pdf("img/snow-map.pdf")
# SnowMap(density=TRUE, main="Snow's Cholera Map, Death Intensity")
# dev.off()

#-----------------------------------------------------------------------

# TODO contar o número de gráficos por "sed" da palana "Nome".
# TODO criar uma árvore com o igraph da organização dos gráficos.
