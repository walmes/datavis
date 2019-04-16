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

#-----------------------------------------------------------------------
# Carrega o pacote.

# # https://github.com/rpradosiqueira/brazilmaps
# library(brazilmaps)
# ls("package:brazilmaps")

library(rgdal)
library(geojsonio)
library(tidyverse)

#-----------------------------------------------------------------------
# Mapa do Brasil com ggplot2.

#-----------------------------------------------------------------------
# Para dos minicípios do MS.

#-----------------------------------------------------------------------
#

#-----------------------------------------------------------------------
# Bairros de Curitiba.

# KML foi gerado a partir de arquivos shape.
kml <- "~/Dropbox/miti/projeto/bairros-cwb.kml"
cwb <- geojson_read(kml, method = "local", what = "sp")
class(cwb)

ggplot() +
    geom_polygon(data = cwb,
                 mapping = aes(x = long,
                               y = lat,
                               # fill = area,
                               group = id),
                 alpha = 0.3,
                 # fill = "#F6E8C3",
                 colour = "black",
                 size = 0.25) +
    theme_light() +
    coord_map()

head(cwb@data)

#-----------------------------------------------------------------------
# Estados do Brasil.

# ATTENTION: Baixar os KML dos site: http://www.gmapas.com/poligonos-ibge.

kml <- "http://www.gmapas.com/poligonos-ibge/poligonos-estados-do-brasil/Estados.kml?attredirects=0&d=1"
br <- geojson_read(kml, method = "local", what = "sp")
class(br)

ggplot() +
    geom_polygon(data = br,
                 mapping = aes(x = long,
                               y = lat,
                               group = id),
                 alpha = 0.3,
                 fill = "blue",
                 colour = "black",
                 size = 0.25) +
  # theme_void() +
  coord_map()

head(br@data)

#-----------------------------------------------------------------------
# Municípios do Paraná.

kml <- "http://www.gmapas.com/poligonos-ibge/poligonos-municipios-ibge-parana/Municipios_PR.kml?attredirects=0&d=1"
pr <- geojson_read(kml, method = "local", what = "sp")
class(pr)

ggplot() +
    geom_polygon(data = pr,
                 mapping = aes(x = long,
                               y = lat,
                               group = id),
                 alpha = 0.3,
                 fill = "red",
                 colour = "black",
                 size = 0.25) +
  theme_light() +
  coord_map()

head(pr@data)

#-----------------------------------------------------------------------
# Mapa dos bairros de Curitiba (com shapefiles).

cwb <- readOGR("/home/walmes/Projects/TCC_Neto_Brasil_ImovelWeb/R/shp",
               use_iconv = TRUE,
               encoding = "UTF-8",
               layer = "DIVISA_DE_BAIRROS")
cwb <- spTransform(cwb,
                   CRS("+proj=longlat +datum=WGS84"))
class(cwb)

ggplot() +
    geom_polygon(data = cwb,
                 aes(x = long,
                     y = lat,
                     group = id),
                 alpha = 0.3,
                 fill = "#F6E8C3",
                 colour = "black",
                 size = 0.25) +
    coord_map()

#-----------------------------------------------------------------------
# Baixar arquivos shape.
# ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/

url <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2018/Brasil/BR/br_unidades_da_federacao.zip"
download.file(url, basename(url))

unzip(basename(url), exdir = "br_estados")

br <- readOGR("br_estados",
              use_iconv = TRUE,
              encoding = "UTF-8")
br <- spTransform(br,
                  CRS("+proj=longlat +datum=WGS84"))
head(br@data)

br_map <- fortify(br)
class(br_map)
head(br_map)

br_data <- br@data
class(br_data)
head(br_data)

# ATTENTION: tem que usar geom_map() e não geom_polygon() para que as
# ilhas não fiquem ligadas com as partes continentais.

# ggplot(data = br_data,
#        mapping = aes(fill = NM_REGIAO)) +
ggplot() +
    # geom_polygon(data = br,
    #              aes(x = long,
    #                  y = lat,
    #                  group = id),
    #              alpha = 0.3,
    #              fill = "green",
    #              colour = "black",
    #              size = 0.25) +
    geom_map(
             data = br_map,
             map = br_map,
        mapping = aes(x = long, y = lat, map_id = id),
             alpha = 0.3,
             # fill = "green",
             colour = "black",
             size = 0.25) +
    coord_map()

plot(br)

#-----------------------------------------------------------------------
# Better example.

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimesm <- reshape2::melt(crimes, id = 1)
if (require(maps)) {
    states_map <- map_data("state")
    ggplot(crimes, aes(map_id = state)) +
        geom_map(aes(fill = Murder, group = state),
                 map = states_map) +
        expand_limits(x = states_map$long,
                      y = states_map$lat)

    last_plot() +
        coord_map()

    ggplot(crimesm,
           aes(map_id = state)) +
        geom_map(aes(fill = value, group = state),
                 map = states_map) +
        expand_limits(x = states_map$long, y = states_map$lat) +
        facet_wrap(~variable)
}

str(states_map)
str(crimes)

#-----------------------------------------------------------------------
