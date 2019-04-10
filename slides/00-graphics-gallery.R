#---- read_futbol ------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(ggjoy))

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
    mutate(games = tit + res,
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
    ylab("Altura dos jogadores ingleses (cm)") +
    scale_x_continuous(name = NULL, breaks = NULL) +
    geom_rug(sides = "l") +
    geom_vline(xintercept = 1, linetype = 2)

gg0 <- ggplot(data = data.frame(x = x),
       mapping = aes(x = 1, group = 1, y = x)) +
    geom_boxplot(fill = "orange", alpha = 0.5) +
    ylab("Altura dos jogadores ingleses (cm)") +
    scale_x_continuous(name = NULL, breaks = NULL) +
    geom_rug(sides = "l")

grid.arrange(gg1, gg0, nrow = 1)

cap <- "Distribuição da altura dos goleiros."

#---- height_joy -------------------------------------------------------

suppressPackageStartupMessages(library(ggridges))

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
    ylab("Altura dos jogadores (cm)") +
    xlab("Função em campo")

grid.arrange(gg + geom_boxplot(fill = "yellow", alpha = 0.5),
             gg + geom_violin(fill = "cyan", alpha = 0.5),
             ncol = 1)

cap <- "Distribuição da altura dos jogadores por função em campo."

#-----------------------------------------------------------------------

library(HistData)
pdf("img/snow-map.pdf")
SnowMap(density=TRUE, main="Snow's Cholera Map, Death Intensity")
dev.off()

#-----------------------------------------------------------------------

# TODO contar o número de gráficos por "sed" da palana "Nome".
# TODO criar uma árvore com o igraph da organização dos gráficos.
