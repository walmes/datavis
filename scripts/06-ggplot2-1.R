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
# Solução dos exercícios propostos nos slides sobre `ggplot2`.

# TODO: ver esses gráficos ->
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

#-----------------------------------------------------------------------
# Carrega o pacote.

library(tidyverse)

#-----------------------------------------------------------------------
# Dados sobre apartamentos à venda no centro de Curitiba.

url <- "http://leg.ufpr.br/~walmes/data/ap-ven-centro-cwb-20161105.csv"
tb <- read_csv2(file = url,
               col_names = TRUE,
               quote = "",
               comment = "#")
class(tb)
glimpse(tb)

#-----------------------------------------------------------------------
# Gráficos para representar a distribuição de valores.

#--------------------------------------------
# Histogramas.

ggplot(data = tb, mapping = aes(x = preco)) +
    geom_histogram()

gg_h <- ggplot(data = tb, mapping = aes(x = preco)) +
    xlab("Preço de anúncio do imóvel (R$)")

gg_h +
    geom_histogram(fill = "orange", color = "black") +
    ylab("Frequência absoluta")

gg_h +
    geom_histogram(bins = 10,
                   fill = "violet",
                   color = "black")

gg_h +
    geom_histogram(binwidth = 100,
                   fill = "seagreen",
                   color = "black")

gg_h +
    geom_histogram(breaks = seq(0, 5000, by = 250),
                   fill = "red2",
                   color = "black")

gg_h +
    geom_histogram(mapping = aes(y = ..density..),
                   fill = "orange", color = "black") +
    ylab("Densidade")

gg_h +
    scale_x_log10() +
    geom_histogram(mapping = aes(y = ..density..),
                   fill = "orange", color = "black") +
    geom_density(color = "blue", size = 1) +
    ylab("Densidade")

ggplot(data = tb, mapping = aes(x = preco)) +
    geom_freqpoly()

library(esquisse)

esquisser(tb)

ggplot(tb) +
    aes(x = metros, y = preco, colour = quartos) +
    geom_point(size = 2.5) +
    scale_color_viridis_c(option = "viridis", direction = 1) +
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10") +
    ggthemes::theme_few()

library(DataExplorer)

plot_histogram(tb)
plot_scatterplot(tb, by="preco")

#--------------------------------------------
# Densidade empírica.

gg_h +
    geom_density(bw = 20,
                 kernel = "triangular",
                 fill = "red2",
                 alpha = 0.5,
                 color = "black") +
    geom_rug() +
    ylab("Densidade")

filter(tb, preco > 2000) %>%
    arrange(preco)

ggplot(data = filter(tb, preco > 2000),
       mapping = aes(x = preco)) +
    geom_density(bw = 25,
                 kernel = "triangular",
                 fill = "red2",
                 alpha = 0.5,
                 color = "black") +
    geom_rug()

#--------------------------------------------
# Densidade empírica (ou relativa) acumulada.

gg_h +
    stat_ecdf() +
    geom_rug() +
    ylab("Frequência relativa acumulada")

gg_h +
    stat_ecdf() +
    scale_x_log10() +
    geom_rug() +
    ylab("Frequência relativa acumulada")

ggplot(data = tb,
       mapping = aes(x = log(metros))) +
    stat_ecdf(mapping = aes(colour = "Empírica")) +
    stat_function(mapping = aes(colour = "Teórica"),
                  fun = pnorm,
                  args = list(mean = mean(log(tb$metros), na.rm = TRUE),
                              sd = sd(log(tb$metros), na.rm = TRUE))) +
    scale_colour_manual(name = "Curva",
                        breaks = c("Empírica", "Teórica"),
                        values = c("cyan", "orange")) +
    geom_rug() +
    xlab("Área do imóvel (log neperiano)") +
    ylab("Frequência relativa acumulada") +
    theme(legend.position = c(0.98, 0.5),
          legend.justification = c(1, 0.5))

#--------------------------------------------
# Gráficos de barras.

ggplot(data = tb,
       mapping = aes(x = banheiros)) +
    geom_bar(fill = "green3", color = "black") +
    stat_bin(binwidth = 1,
             mapping = aes(label = ..count..),
             geom = "text",
             vjust = -1) +
    xlab("Número de banheiros") +
    ylab("Frequência absoluta") +
    scale_x_continuous(breaks = 1:max(tb$banheiros))

ggplot(data = tb,
       mapping = aes(x = quartos)) +
    geom_bar() +
    xlab("Número de quartos") +
    ylab("Frequência absoluta") +
    scale_x_continuous(breaks = 1:max(tb$quartos)) +
    coord_flip()

#--------------------------------------------
# Gráfico de setores.

ggplot(data = tb,
       mapping = aes(x = factor(1),
                     fill = factor(quartos))) +
    labs(fill = "Quartos") +
    geom_bar(width = 0.75) +
    coord_polar(theta = "y") +
    xlab(NULL) +
    ylab(NULL) +
    theme_minimal()

#--------------------------------------------
# Gráfico de caixas e bigodes.

ggplot(data = tb,
       mapping = aes(x = 1, y = metros)) +
    scale_y_sqrt() +
    geom_boxplot()

#--------------------------------------------
# Gráfico de violino.

ggplot(data = tb,
       mapping = aes(x = 1, y = metros)) +
    scale_y_sqrt() +
    geom_violin()

ggplot(data = tb,
       mapping = aes(x = 1, y = metros)) +
    scale_y_sqrt() +
    geom_jitter(pch = 1) +
    geom_violin(alpha = 0.4)

#--------------------------------------------
# Gráfico de pontos empilhados.

set.seed(4321)
tb_sample <- tb %>%
    sample_n(size = 40)

ggplot(data = tb_sample,
       aes(x = banheiros)) +
    geom_dotplot(stackdir = "up")

ggplot(data = tb_sample,
       aes(x = banheiros)) +
    geom_dotplot(stackdir = "center") +
    coord_flip()

# ATTENTION: cada ponto é uma observação. Então o dotplot é uma
# representação gráfica apropriada para amostras pequenas. Para amostras
# grandes, um ponto terá que representar uma dezena/centena/milhar de
# observações e as contas precisam ser feitas "na mão".

cell <- ceiling(nrow(tb)/100)
width <- 50
bks <- seq(from = 0,
           to = ceiling(max(tb$preco)/width) * width,
           by = width)
mds <- head(bks, n = -1) + width/2

tb_dot <- tb %>%
    transmute(class = cut(preco, breaks = bks, labels = mds)) %>%
    mutate(mds = as.integer(as.character(class))) %>%
    count(mds) %>%
    mutate(freq = round(n/cell)) %>%
    group_by(mds) %>%
    summarise(x = list(rep(mds, times = freq))) %>%
    select(x) %>%
    unnest(x)
tb_dot

ggplot(data = tb_dot,
       aes(x = x)) +
    geom_dotplot(binwidth = width) +
    scale_y_continuous(NULL, breaks = NULL) +
    scale_x_continuous(breaks = mds)

# DANGER: muito trabalho para uma exibição pouco precisa. Considere um
# histograma (geom_histogram) ou gráfico de barras (geom_bar).

ggplot(data = tb,
       mapping = aes(x = preco)) +
    geom_histogram(breaks = bks,
                   color = "black",
                   fill = "red3")

#-----------------------------------------------------------------------
# Gráficos para relação entre variáveis.

# Importação dos dados de jogadores de futebol.
url <- "http://leg.ufpr.br/~walmes/data/euro_football_players.txt"
browseURL(url)

tb <- read_tsv(file = url,
               col_names = TRUE,
               quote = "",
               comment = "#")
class(tb)
glimpse(tb)

# OBS: NA na variável `apps` indica que o jogador não entrou em
# campo. Em função disso, as variáveis de desempenho estarão com NA
# porque não houve "exposição" para que fossem mensuradas.

# Estatísticas sobre os valores ausentes.
tb %>%
    group_by(is.na(apps)) %>%
    summarise_all(~sum(is.na(.))) %>%
    rename(missing = "is.na(apps)") %>%
    column_to_rownames("missing") %>%
    t()

# Tabela 2 só com jogadores que estiveram em campo.
# O NA no intervalo de variáveis é substituido por 0.
tb2 <- tb %>%
    filter(!is.na(apps)) %>%
    mutate_at(vars(goal:mom), replace_na, replace = 0)
glimpse(tb2)

# Ocorrência dos valores ausentes em cada variável.
tb2 %>%
    summarise_all(~sum(is.na(.))) %>%
    t()

#--------------------------------------------
# Criação de variáveis via manipulação de expressões regulares.

# 1. Criar a posição em que joga.
# 2. Criar o número de aparições.

# Principal posição em que o jogador atua. Teste da regex.
tb$pos %>%
    unique() %>%
    str_replace(pattern = "^([A-Z]+)\\(?.*$",
                replacement = "\\1") %>%
    unique()

# Para determinas o número de aparições:
# 1. São duas situações:
#    9: jogou 9 vezes como titular.
#    9(2): também jogou 2 vezes como substituto.
# 2. Para não usar regex, quebrar no parentese que abre.
# 3. Do lado direito são os jogos como titular, do esquedo os como
#    substituto.
# 4. Os NA dos jogos como substituto trocar por 0.
# 5. Somar o número de jogos como titular e substituto.

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

#--------------------------------------------
# Categórica x categórica.

ggplot(data = tb2,
       mapping = aes(x = fct_infreq(position))) +
    geom_bar()

tb2 %>% count(yel)
tb2 %>% count(red)

ggplot(data = tb2,
       mapping = aes(x = red)) +
    geom_bar()

ggplot(data = tb2,
       mapping = aes(x = yel)) +
    geom_bar()

tb2 <- tb2 %>%
    mutate(yel2 = fct_other(factor(yel),
                            keep = as.character(0:5),
                            other_level = ">5"))

ggplot(data = tb2,
       mapping = aes(x = yel2)) +
    geom_bar()

# Mapeamento estético.
gg <- ggplot(data = tb2,
             mapping = aes(x = fct_reorder(position, yel, length),
                           fill = yel2)) +
    scale_fill_discrete(name = "Cartões\namarelos")

# Barras lado a lado.
gg + geom_bar(position = "dodge")

# Barras empilhadas.
gg + geom_bar(position = "stack")

# Barras esticadas para preencher.
gg + geom_bar(position = "fill")

gg <- ggplot(data = tb2,
             mapping = aes(x = yel2)) +
    geom_bar()

# Barras com grupos separados.
gg +
    facet_wrap(facets = ~position)

# Anotações sobre as barras.
gg +
    facet_wrap(facets = ~position) +
    geom_text(mapping = aes(y = ..count..,
                            label = ..count..),
              stat = "count",
              vjust = -0.5)

gg <- ggplot(data = tb2,
       mapping = aes(x = 1,
                     fill = fct_rev(yel2))) +
    scale_fill_discrete(name = "Cartões\namarelos",
                        breaks = fct_unique(tb2$yel2))

# Barras representando a proporção.
gg +
    geom_bar(color = "black", position = "fill")

# Gráfico de setores com a proporção. Anotações feitas com variáveis
# calculadas acessadas por `..?..`.
gg +
    geom_bar(color = "black", position = "fill") +
    geom_text(mapping = aes(y = ..count../sum(..count..),
                            label = ..count..),
              position = position_stack(vjust = 0.5),
              stat = "count") +
    coord_polar(theta = "y") +
    xlab(NULL) +
    ylab(NULL) +
    theme_minimal()

# Anotações calculadas para por dentro dos setores.
tb_aux <- tb2 %>%
    count(yel2) %>%
    mutate(prop = n/sum(n),
           label = sprintf("%d\n(%0.1f%%)", n, 100 * prop))
tb_aux

gg <- ggplot(data = tb_aux,
             mapping = aes(x = 0, y = prop, fill = yel2)) +
    scale_fill_discrete(name = "Cartões\namarelos",
                        breaks = fct_unique(tb2$yel2)) +
    geom_col(color = "black") +
    xlab(NULL) +
    ylab(NULL)

gg +
    geom_text(mapping = aes(label = label),
              position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y")

# Gráfico de rosca (donut plot).
gg +
    xlim(c(-1.5, 0.5)) +
    geom_text(mapping = aes(label = label),
              position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y")

# Gráfico de setores separados por posição.
ggplot(data = tb2,
       mapping = aes(x = 0,
                     fill = fct_rev(yel2))) +
    geom_bar(color = "black", position = "fill") +
    scale_fill_discrete(name = "Cartões\namarelos",
                        breaks = fct_unique(tb2$yel2)) +
    facet_wrap(facets = ~position) +
    coord_polar(theta = "y") +
    xlab(NULL) +
    ylab(NULL)

# TIP: Para fazer gráficos de mosaico, carregar o pacote `ggmosaic` e
# usar a `geom_mosaic`. Exemplos em
# https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html.

# Muda a escala de cores.
gg <- ggplot(data = tb2,
             mapping = aes(x = 0, fill = position)) +
    geom_bar(color = "black", position = "fill") +
    coord_polar(theta = "y") +
    xlab(NULL) +
    ylab(NULL)
gg

gg +
    scale_fill_manual(values = c("red", "green", "purple",
                                 "orange", "cyan", "blue"))

library(RColorBrewer)

gg + scale_fill_brewer(palette = "Dark2")
gg + scale_fill_brewer(palette = "Blues")
gg + scale_fill_brewer(palette = "Reds")

#--------------------------------------------
# Métrica x métrica.

gg <- ggplot(data = tb2,
             mapping = aes(x = cm, y = kg)) +
    xlab("Altura do jogador (cm)") +
    ylab("Peso do jogador (kg)")

gg + geom_point()
gg + geom_point(pch = 1)
gg + geom_point(alpha = 0.3)
gg + geom_point() + geom_density2d()
gg + geom_jitter()

gg + geom_hex()
gg + geom_hex(bins = 50)

gg + geom_bin2d()
gg + geom_bin2d(bins = 50)

gg +
    geom_bin2d(bins = 50) +
    scale_fill_gradientn(colours = rev(cm.colors(6)))

gg + geom_point(mapping = aes(size = age))
gg + geom_jitter(mapping = aes(alpha = age))

gg +
    geom_jitter(mapping = aes(color = age)) +
    scale_color_gradient(low = "blue", high = "red")

gg +
    geom_jitter(mapping = aes(color = age), size = 2) +
    # scale_color_gradientn(colours = heat.colors(5))
    scale_color_gradientn(colours = rainbow(5))

gg +
    geom_jitter() +
    geom_smooth()

gg +
    geom_jitter() +
    geom_quantile(quantiles = c(0.1, 0.9))

gg +
    geom_jitter() +
    geom_smooth(span = 0.1)

gg +
    geom_jitter() +
    geom_smooth(method = "lm", formula = y ~ poly(x, degree = 6))

gg +
    geom_jitter() +
    geom_smooth(mapping = aes(color = "Polinômio(k = 3)"),
                method = "lm",
                formula = y ~ poly(x, degree = 3),
                se = FALSE) +
    geom_smooth(mapping = aes(color = "Reg. local"),
                span = 0.1,
                method = "loess",
                se = TRUE) +
    scale_color_manual(name = "Modelo",
                       values = c("cyan", "orange")) +
    theme(legend.position = c(0.95, 0.05),
          legend.justification = c(1, 0))

gg +
    geom_jitter() +
    geom_smooth(span = 0.5, method = "loess") +
    facet_wrap(facets = ~position)

gg +
    geom_jitter() +
    geom_smooth(span = 0.5, method = "loess") +
    facet_wrap(facets = ~cut_number(age, n = 8))

gg +
    geom_jitter() +
    geom_smooth(span = 0.5, method = "loess") +
    facet_wrap(facets = ~cut_width(age, width = 5))

# Os artilheiros.
tb_top <- tb2 %>%
    top_n(goal, n = 40)

ggplot(data = tb_top,
       mapping = aes(x = fct_reorder(name, goal),
                     y = goal,
                     shape = position)) +
    geom_line(mapping = aes(group = 1), color = "black") +
    geom_point(size = 3) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = c(0.95, 0.05),
          legend.justification = c(1, 0),
          legend.direction = "horizontal")

ggplot(data = tb_top,
       mapping = aes(x = fct_reorder(name, goal),
                     y = goal,
                     shape = position)) +
    geom_point(size = 3) +
    geom_label(mapping = aes(label = team),
               hjust = 0,
               nudge_y = 0.1) +
    scale_y_continuous(breaks = min(tb2$goal):max(tb2$goal)) +
    coord_flip() +
    theme(legend.position = c(0.95, 0.05),
          legend.justification = c(1, 0),
          legend.direction = "horizontal")

# Os mais altos.
tb_top <- tb2 %>%
    top_n(cm, n = 40)

ggplot(data = tb_top,
       mapping = aes(x = fct_reorder(name, cm),
                     y = cm,
                     shape = position)) +
    geom_point(size = 3) +
    geom_label(mapping = aes(label = country),
               hjust = 0,
               nudge_y = 0.1) +
    coord_flip() +
    theme(legend.position = c(0.95, 0.05),
          legend.justification = c(1, 0),
          legend.direction = "horizontal")

#--------------------------------------------
# Métrica x categórica.

gg <- ggplot(data = tb2,
             mapping = aes(x = fct_reorder(position, cm,
                                           mean, na.rm = TRUE),
                           y = cm))

gg + geom_jitter(width = 0.1)

gg + geom_jitter(mapping = aes(size = age), width = 0.1, alpha = 0.5)

gg + geom_boxplot(fill = "orange")

gg + geom_violin(fill = "purple")
gg + geom_violin(fill = "purple",
                 trim = FALSE,
                 kernel = "cosine")

gg +
    geom_violin(fill = "purple", trim = FALSE) +
    stat_summary(fun.y = mean, geom = "point", size = 2, color = "white")

gg +
    geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.2)

# TIP: Para fazer Joy plot, veja o pacote `ggjoy`, `geom_joy()`.
# http://www.sthda.com/english/articles/16-r-packages/30-ggjoy-create-a-ggplot2-based-joyplots/
# https://stackoverflow.com/questions/52034747/plot-only-one-side-half-of-the-violin-plot

tb_norm <- tb2 %>%
    filter(is.finite(cm)) %>%
    group_by(position) %>%
    do({
        d <- density(x = .$cm)
        m <- mean(.$cm)
        s <- sd(.$cm)
        x <- d$x
        y <- dnorm(x, m, s)
        tibble(x, y_norm = y, y_kernel = d$y)
    }) %>%
    ungroup()

tb_norm <- tb_norm %>%
    gather(key = "dist", value = "density", y_norm, y_kernel)

ggplot(data = tb_norm,
       mapping = aes(x = x)) +
    geom_area(mapping = aes(y = density, fill = dist),
              position = "dodge",
              alpha = 0.5,
              color = "black") +
    facet_wrap(facets = ~position, nrow = 1) +
    coord_flip()

tb_norm <- tb_norm %>%
    mutate(u = as.integer(factor(position)),
           density_sc = 0.9 * density/max(density) + u - 1)

ggplot(data = tb_norm,
       mapping = aes(x = x, fill = position)) +
    geom_polygon(mapping = aes(y = density_sc, linetype = dist),
                 alpha = 0.4,
                 color = "black") +
    scale_y_continuous(breaks = unique(tb_norm$u) - 1,
                       labels = unique(tb_norm$position)) +
    coord_flip()

#-----------------------------------------------------------------------
# Importação dos dados sobre a ocorrência das ninfas de percevejo em
# soja.

url <- "http://leg.ufpr.br/~walmes/data/ninfas.txt"
tb <- read_tsv(url)
glimpse(tb)

#-----------------------------------------------------------------------

ggplot(data = tb,
       mapping = aes(x = superior)) +
    geom_histogram()

ggplot(data = tb,
       mapping = aes(x = superior)) +
    geom_histogram(fill = "orange",
                   color = "black",
                   # bins = 10,
                   # binwidth = 30,
                   breaks = seq(0, 500, by = 50)) +
    geom_rug() +
    facet_wrap(facets = ~variedade)

tb_tot <- tb %>%
    group_by(variedade) %>%
    summarise(tot = sum(superior))

ggplot(data = tb,
       mapping = aes(x = superior)) +
    geom_histogram(fill = "orange",
                   color = "black",
                   # bins = 10,
                   # binwidth = 30,
                   breaks = seq(0, 500, by = 50)) +
    geom_rug() +
    facet_wrap(facets = ~variedade) +
    geom_text(data = tb_tot,
              mapping = aes(x = 300,
                            y = 15,
                            label = tot)) +
    theme_light()

ggplot(data = tb,
       mapping = aes(x = superior)) +
    geom_density(fill = "violet",
                 kernel = "rectangular",
                 bw = 50) +
    geom_rug() +
    facet_wrap(facets = ~variedade)

ggplot(data = tb,
       mapping = aes(x = superior)) +
    stat_ecdf() +
    geom_rug() +
    facet_wrap(facets = ~variedade)

#-----------------------------------------------------------------------

ggplot(data = tb,
       mapping = aes(x = variedade)) +
    geom_bar()

#-----------------------------------------------------------------------

tb_aux <- tb %>%
    group_by(variedade) %>%
    summarise(tot = sum(superior))

ggplot(data = tb_aux,
       mapping = aes(x = fct_reorder(variedade, tot),
                     y = tot)) +
    geom_col(fill = "#beff70") +
    geom_text(mapping = aes(x = variedade,
                            y = tot - 100, # <- melhorar isso!
                            label = tot)) +
    xlab("Variedade de soja") +
    ylab("Total de ninfas no terço superior") +
    coord_flip()

tb_aux <- tb %>%
    group_by(variedade, data) %>%
    summarise(tot = sum(superior))

gg1 <- ggplot(data = tb_aux,
       mapping = aes(x = data,
                     y = tot)) +
    geom_point(color = "blue4") +
    geom_line(color = "blue4") +
    facet_wrap(facets = ~variedade) +
    xlab("Variedade de soja") +
    ylab("Total de ninfas no terço superior")

tb_aux2 <- tb_aux %>%
    group_by(variedade) %>%
    top_n(tot, n = 1)

gg1 +
    ylim(c(0, 1100)) +
    geom_label(data = tb_aux2,
               mapping = aes(x = data, y = tot, label = tot),
               hjust = 0, vjust = 0)

tb_aux3 <- tb %>%
    gather(key = "terco",
           value = "quant",
           superior:inferior) %>%
    group_by(data, variedade, terco) %>%
    summarise(tot = sum(quant))
glimpse(tb_aux3)

ggplot(data = tb_aux3,
       mapping = aes(x = data,
                     y = tot,
                     color = terco)) +
    facet_wrap(facets = ~variedade, nrow = 2) +
    geom_point() +
    geom_line() +
    xlab("Data de avaliação") +
    ylab("Total de ninfas") +
    labs(color = "Terço") +
    ggtitle(label = "Ninfas de percevejo em soja",
            subtitle = "Safra 2010/2011") +
    theme_light() +
    theme(legend.justification = c(1, 0),
          legend.position = c(0.95, 0.2))

#-----------------------------------------------------------------------

ggplot(data = faithfuld,
       mapping = aes(x = waiting,
                     y = eruptions,
                     z = density)) +
    geom_contour(color = "black")

ggplot(data = faithfuld,
       mapping = aes(x = waiting,
                     y = eruptions,
                     fill = density)) +
    geom_tile()

ggplot(data = faithfuld,
       mapping = aes(x = waiting,
                     y = eruptions,
                     fill = density)) +
    geom_raster() +
    scale_fill_gradientn(colours = rev(rainbow(6)))

#-----------------------------------------------------------------------
# WALMES TODO! FIXME!

help(geom_map, h = "html")

ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

values <- data.frame(
    id = ids,
    value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
)

positions <- data.frame(
    id = rep(ids, each = 4),
    x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
          0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
    y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
          2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)

ggplot(values) +
    geom_map(aes(map_id = id), map = positions) +
    expand_limits(positions)
ggplot(values, aes(fill = value)) +
    geom_map(aes(map_id = id), map = positions) +
    expand_limits(positions)
ggplot(values, aes(fill = value)) +
    geom_map(aes(map_id = id), map = positions) +
    expand_limits(positions) + ylim(0, 3)

# Better example
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimesm <- reshape2::melt(crimes, id = 1)
if (require(maps)) {
    states_map <- map_data("state")
    ggplot(crimes, aes(map_id = state)) +
        geom_map(aes(fill = Murder), map = states_map) +
        expand_limits(x = states_map$long, y = states_map$lat)

    last_plot() + coord_map()
    ggplot(crimesm, aes(map_id = state)) +
        geom_map(aes(fill = value), map = states_map) +
        expand_limits(x = states_map$long, y = states_map$lat) +
        facet_wrap( ~ variable)
}

#-----------------------------------------------------------------------
# Anotações e geometria adicional.

# WALMES TODO FIXME!

# Linhas verticais/horizontais.
# geom_vline, geom_abline, geom_hline.

# Curvas.
# stat_funtion, stat_summary

# Intervalos/segmentos.
# stat_errorbar

# Polígonos.
# geom_polygon

# Texto.
# geom_text, geom_label

# TIP: Para fazer radar (spider) plot com ggplot, veja o pacote
# `ggradar`.
#
# https://www.ggplot2-exts.org/ggradar.html
# https://medium.com/@rhdzmota/alcohol-and-radar-plots-in-r-with-ggplot2-9ba7ad8c92c

# TIP: Para fazer parallel plot veja o pacote `GGally`.
# https://rdrr.io/cran/GGally/man/ggparcoord.html
# https://datascience.blog.wzb.eu/2016/09/27/parallel-coordinate-plots-for-discrete-and-categorical-data-in-r-a-comparison/

# TIP: Para fazer scatter plot matrix e correlograma use o `GGally`.
# http://www.sthda.com/english/wiki/ggally-r-package-extension-to-ggplot2-for-correlation-matrix-and-survival-plots-r-software-and-data-visualization

# TIP: Para fazer correlogramas use o `ggcorrplot` ou `ggcorr`.
# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
# https://briatte.github.io/ggcorr/

# TIP: Para fazer dendogramas use o `ggdendro`.
# http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning

# TIP: Heatmap com ggplot usando geom_tile.
# https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/

# TODO: gráficos de coordenadas polares.
# http://rpubs.com/htejero/212368


demo(plots, package = "DescTools")
