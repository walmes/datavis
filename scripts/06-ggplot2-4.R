#-----------------------------------------------------------------------
# Manipulação e visualização de dados: a abordagem tidyverse
# http://leg.ufpr.br/~walmes/cursoR/data-vis/
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                leg.ufpr.br/~walmes · github.com/walmes
#                                        walmes@ufpr.br · @walmeszeviani
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-abr-16 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#=======================================================================
# Solução dos exercícios propostos nos slides sobre `ggplot2`.

#-----------------------------------------------------------------------
# Pacotes.

rm(list = ls())
library(geojsonio)
library(sp)
library(gridExtra)
library(tidyverse)
library(broom)

#-----------------------------------------------------------------------
# Importar arquivos com dados sobre imóveis a venda em Curitiba.

url <- "http://leg.ufpr.br/~walmes/data/TCC_Brasil_Neto/ImoveisWeb-Realty.csv"
# dir.create("../tempdir")
path <- "../tempdir/ImoveisWeb-Realty.csv"

if (!file.exists(path)) {
    download.file(url, path)
}

imo <- read_csv2(path, locale = locale(encoding = "latin1"))
attr(imo, "spec") <- NULL
str(imo, vec.len = 3, nchar.max = 30)

# Aplicação de filtros.
imo <- imo %>%
    filter(type == "Apartamento",
           between(price, 10^4, 10^7),
           between(usefulArea, 1.1 * 10^1, 3 * 10^3),
           bedroom <= 20,
           between(lon, -49.40, -49.15),
           between(lat, -25.65, -25.30)) %>%
    select(lat,
           lon,
           pictures,
           bedroom,
           usefulArea,
           suite,
           bathroom,
           garage,
           price)

#-----------------------------------------------------------------------
# A distribuição das variáveis numéricas.

ggplot(imo) +
    aes(x = price) +
    geom_density()

ggplot(imo) +
    aes(x = usefulArea) +
    geom_density()

ggplot(imo) +
    aes(x = bedroom) +
    geom_bar()

#-----------------------------------------------------------------------
# Distribuição espacial.

ggplot(imo) +
    aes(x = lon, y = lat) +
    geom_point() +
    coord_map()

ggplot(imo) +
    aes(x = lon, y = lat) +
    geom_hex(bins = 60) +
    coord_equal()

ggplot(imo) +
    aes(x = lon, y = lat) +
    geom_point(pch = 1, alpha = 0.1) +
    geom_density2d(color = "red", size = 0.75) +
    coord_equal()

#-----------------------------------------------------------------------
# Relação entre variáveis.

ggplot(imo) +
    aes(usefulArea, price) +
    geom_point()

ggplot(imo) +
    aes(usefulArea, price) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10()

ggplot(imo) +
    aes(usefulArea, price, color = bedroom) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_distiller(palette = "Spectral", direction = -1)

ggplot(imo) +
    aes(usefulArea, price) +
    facet_wrap(~bedroom) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10()

#-----------------------------------------------------------------------
# Importar polígono dos bairros.

# KML foi gerado a partir de arquivos shape.
url <- "http://leg.ufpr.br/~walmes/data/bairros-cwb.kml"
# dir.create("../tempdir")
path <- "../tempdir/bairros-cwb.kml"

if (!file.exists(path)) {
    download.file(url, path)
}

cwb <- geojson_read(url,
                    method = "local",
                    what = "sp",
                    use_iconv = TRUE,
                    encoding = "utf-8")
class(cwb)

# Acerta caracteres mal formados.
cwb$NOME <- as.character(cwb$NOME)
cwb$NOME <- iconv(cwb$NOME, to = "ASCII//TRANSLIT") %>%
    str_replace("\\?", "")

# Cria a tabela com os valores para cada área.
cwb_data <- as_tibble(cwb@data)
str(cwb_data)

# Cria a tabela com as delimitações dos polígonos.
cwb_poly <- as_tibble(tidy(cwb)) %>%
    rename(region = id)

# Gráfico com as delimitações dos bairros.
ggplot(data = cwb_poly,
       mapping = aes(x = long,
                     y = lat,
                     map_id = region)) +
           geom_map(map = cwb_poly,
                    alpha = 0.3,
                    colour = "black",
                    size = 0.25) +
           theme_light() +
           coord_map()

# Considerando que a ordem está valendo.
cwb_data$region <- unique(cwb_poly$region)

# Mapa temático mostrando a área dos bairros.
gg_cwb <-
    ggplot(data = cwb_data,
           mapping = aes(map_id = region)) +
    geom_map(map = cwb_poly,
             mapping = aes(fill = log(AREA)),
             alpha = 0.3,
             colour = "black",
             size = 0.25) +
    scale_fill_distiller(palette = 4, direction = 1) +
    expand_limits(x = cwb_poly$long, y = cwb_poly$lat) +
    theme_light() +
    coord_map()
gg_cwb

#-----------------------------------------------------------------------
# Cortar as coordenadas dentro dos polígonos de cada bairro.

# Cria objeto de classe `sp`.
xy <- imo %>%
    select(lon, lat)
coordinates(xy) <- ~lon + lat
proj4string(xy) <- proj4string(cwb)

# Classe `sp`.
class(xy)
class(cwb)

# Variáveis que será criada.
names(cwb)
length(cwb[, "NOME"])
class(cwb[, "NOME"])

# Faz a junção.
imo$bairro <- over(xy, cwb[, "NOME"])$NOME

# Para criar a coluna `region`.
imo <- inner_join(imo,
                  cwb_data[, c("NOME", "region")],
                  by = c("bairro" = "NOME"))
imo

# Sobrepõe os pontos sobre o mapa.
ggplot(data = cwb_data,
       mapping = aes(map_id = region)) +
    geom_map(map = cwb_poly,
             mapping = aes(fill = log(AREA)),
             alpha = 0.3,
             colour = "black",
             size = 0.25) +
    scale_fill_distiller(palette = 4, direction = 1) +
    expand_limits(x = cwb_poly$long, y = cwb_poly$lat) +
    theme_light() +
    coord_map() +
    geom_point(data = imo,
               mapping = aes(x = lon, y = lat),
               alpha = 0.2)

#-----------------------------------------------------------------------
# Preço do metro quadrado médio por bairro.

# Determina o valor do metro quadrado.
imo <- imo %>%
    mutate(price_area = price/usefulArea)

mean <- function(x, ..., na.rm = TRUE) {
    base::mean(x, ..., na.rm = na.rm)
}

# Calcula um conjunto de estatísticas para cada bairro.
imo_val <- imo %>%
    group_by(bairro, region) %>%
    summarise(price_area = mean(price_area),
              bedroom = mean(bedroom),
              garage = mean(garage),
              bathroom = mean(bathroom),
              usefulArea = mean(usefulArea),
              price = mean(price),
              n_imo = n())

# Apenas para exibir a tabela ordenada.
imo_val %>%
    arrange(desc(price_area))

# Dotplot de Cleveland ordenado.
ggplot(imo_val) +
    aes(y = fct_reorder(bairro, price_area),
        x = price_area) +
    geom_point() +
    expand_limits(x = c(0, NA)) +
    geom_text(aes(label = bairro),
              hjust = 1,
              size = 2.5,
              nudge_x = -20)

# Prototipa para mostrar os mapas temáticos.
gg0 <- ggplot(data = imo_val,
              mapping = aes(map_id = region)) +
    expand_limits(x = cwb_poly$long, y = cwb_poly$lat) +
    theme_light() +
    coord_map()

# Número de imóveis à venda.
gg1 <-
    gg0 +
    geom_map(map = cwb_poly,
             mapping = aes(fill = n_imo),
             colour = "black",
             size = 0.25) +
    scale_fill_distiller(palette = "Reds", direction = 1)

# Preço médio do metro quadrado.
gg2 <-
    gg0 +
    geom_map(map = cwb_poly,
             mapping = aes(fill = price_area),
             colour = "black",
             size = 0.25) +
    scale_fill_distiller(palette = "Blues", direction = 1)

# Área útil média.
gg3 <-
    gg0 +
    geom_map(map = cwb_poly,
             mapping = aes(fill = usefulArea),
             colour = "black",
             size = 0.25) +
    scale_fill_distiller(palette = "Greens", direction = 1)

# Exibe os 3 gráficos.
grid.arrange(gg1, gg2, gg3, nrow = 1)

#-----------------------------------------------------------------------
# Alvarás.

url <- "http://leg.ufpr.br/~walmes/data/TCC_Brasil_Neto/alvaras_final.csv"
path <- "../tempdir/alvaras_final.csv"

if (!file.exists(path)) {
    download.file(url, path)
}

# Tabelas com informação da localização e estabelecimentos comerciais
# com alvarás.
alv <- read_csv2(path, locale = locale(encoding = "latin1"))
attr(alv, "spec") <- NULL
str(alv, vec.len = 3, nchar.max = 30)

# Aplica os filtros para eliminar valores não condizentes.
alv <- alv %>%
    filter(is.finite(lat1),
           between(lon1, -49.40, -49.15),
           between(lat1, -25.65, -25.30)) %>%
    select(cat, lon1, lat1)
alv

#-----------------------------------------------------------------------
# Cortar em classe para fazer a agregração.

# Cria uma malha aproximadamente quadrada para cortar.
k <- 48
cx <- extendrange(c(alv$lon1, imo$lon), f = 0.01)
cx <- seq(cx[1], cx[2], length.out = k)
cy <- extendrange(c(alv$lat1, imo$lat), f = 0.01)
cy <- seq(cy[1], cy[2], length.out = ceiling(4 * k/3))

# Visualiza a malha.
gg0 <-
ggplot(imo) +
    aes(lon, lat) +
    geom_point(color = "cyan") +
    geom_density2d(color = "red", size = 0.75) +
    coord_equal() +
    theme_light() +
    geom_vline(xintercept = cx, size = 0.5, color = "gray80") +
    geom_hline(yintercept = cy, size = 0.5, color = "gray80")
gg1 <-
ggplot(alv) +
    aes(lon1, lat1) +
    geom_point(color = "orange") +
    geom_density2d(color = "blue", size = 0.75) +
    coord_equal() +
    theme_light() +
    geom_vline(xintercept = cx, size = 0.5, color = "gray80") +
    geom_hline(yintercept = cy, size = 0.5, color = "gray80")
grid.arrange(gg0, gg1, nrow = 1)

# Cria a classificação em cédulas da malha para os imóveis.
imo <- imo %>%
    mutate(clon = cut(lon, breaks = cx),
           clat = cut(lat, breaks = cy))

# Cria a classificação em cédulas da malha para os estabelecimentos.
alv <- alv %>%
    mutate(clon = cut(lon1, breaks = cx),
           clat = cut(lat1, breaks = cy)) %>%
    ungroup()

# Agregação dos imóveis por cédula.
imo_cxy <- imo %>%
    group_by(clon, clat) %>%
    summarise(n_imo = n(),
              price_area = mean(price_area)) %>%
    ungroup()

# Agregação dos estabelecimentos por cédula.
alv_cxy <- alv %>%
    group_by(clon, clat) %>%
    summarise(n_alv = n())

# Junção em uma tabela só.
imo_alv <- full_join(imo_cxy, alv_cxy)
imo_alv <- imo_alv %>%
    replace_na(replace = list(n_alv = 0, n_imo = 0))

# Relação entre as quantidades.
ggplot(imo_alv) +
    aes(n_imo, n_alv) +
    geom_point()

# Relação entre o preço do métro quadrado e intensidade de
# estabelecimentos.
ggplot(imo_alv) +
    aes(n_alv, price_area) +
    geom_point() +
    scale_x_log10() +
    geom_smooth(method = "lm")

#-----------------------------------------------------------------------
# Visualização do índice de Moran.

imo_alv <- imo_alv %>%
    mutate(clonf = as.integer(clon),
           clatf = as.integer(clat))


my_pair <- function(i, j) {
    tb_cell <- imo_alv %>%
        filter(between(clonf, i - 1, i + 1),
               between(clatf, j - 1, j + 1))
    u <- which(tb_cell$clonf == i & tb_cell$clatf == j)
    data.frame(mean = ifelse(nrow(tb_cell),
                             mean(tb_cell$price_area[-u]),
                             NA),
               value = ifelse(length(u),
                              tb_cell$price_area[u],
                              NA),
               nnei = ifelse(nrow(tb_cell),
                             nrow(tb_cell) - length(u),
                             NA))
}

crx <- crossing(i = 1:nlevels(imo_alv$clon),
                j = 1:nlevels(imo_alv$clat))

crx <- crx %>%
    mutate(pair = map2(i, j, my_pair))
crx <- crx %>%
    unnest()

ggplot(crx) +
    aes(mean, value) +
    geom_point(pch = 1) +
    geom_smooth(se = FALSE, color = "orange")

#-----------------------------------------------------------------------
