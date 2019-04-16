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

rm(list = ls())
library(rgdal)     # Lê shape.
library(geojsonio) # Lê KML.
library(tidyverse)
library(broom)

#-----------------------------------------------------------------------
# Lê informação extraída do IPEA DATA.

url <- "../data/ipeadata_populacao_estados.txt"
tb <- read_tsv(url, col_types = cols())
attr(tb, "spec") <- NULL
str(tb)

# Passar nome do Estado para caixa alta para fazer junção com a tabela
# de informações geográficas.
tb <- tb %>%
    mutate(estado = str_to_upper(estado))

# Filtra para dados de 2010.
tb_2010 <- tb %>%
    filter(ano == 2010)

#-----------------------------------------------------------------------
# Mapa do Brasil com ggplot2.

# Baixar arquivos shape.
# ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/

# Download do ZIP com arquivos shape.
url <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2018/Brasil/BR/br_unidades_da_federacao.zip"
download.file(url, basename(url))

# Descompacta dentro do diretório de trabalho.
unzip(basename(url), exdir = "br_estados")

# Lê o arquivo para criar o objeto espacial
br <- readOGR("br_estados",
              use_iconv = TRUE,
              encoding = "UTF-8")
class(br)

head(br@data)       # Tabela de dados.
length(br@polygons) # Tabela com os polígonos.
br@proj4string      # Sobre a projeção.

# Tabela com dados.
br_data <- as_tibble(br@data)
br_data <- br_data %>%
    rename(estado = NM_ESTADO, regiao = NM_REGIAO)
head(br_data)

# Tabela com coordenadas de cada polígono.
br_map <- as_tibble(fortify(br))
br_map <- br_map %>%
    rename(region = id)

# Um Estado pode ser composto por mais de um polígono.
length(unique(br_map$region)) # Cada Estado.
length(unique(br_map$group))  # Cada polígono.

# ATTENTION: as variáveis `long`, `lat` e `id` são as variáveis
# esperadas pela função `geom_map()`. O `id` guarda correspondência com
# a variável `id` na tabela de dados.

# Gráfico sem variável vinculada ao preenchimento.
ggplot(data = br_map,
       mapping = aes(x = long, y = lat)) +
    geom_map(mapping = aes(map_id = region),
             map = br_map,
             alpha = 0.3,
             colour = "black",
             size = 0.25) +
    coord_map()

# Supondo que as coisas estão na ordem para criar a chave.
br_data$region <- unique(br_map$region)

# Junta a informação.
br_data_2010 <- inner_join(br_data, tb_2010)

br_data_2010 %>%
    arrange(-prop_urb) %>%
    select(estado, prop_urb)

ggplot(data = br_data_2010,
       mapping = aes(map_id = region)) +
    geom_map(mapping = aes(fill = prop_urb),
             map = br_map,
             colour = "black",
             size = 0.25) +
    scale_fill_distiller(palette = 4, direction = 1) +
    expand_limits(x = br_map$long, y = br_map$lat) +
    coord_map()

# ATTENTION! Como `x` e `y` não são variáveis visuais sendo
# especificadas, para determinar os limites precisa usar
# `expand_limits()`, caso contrário surge o erro:
#
#   Error in data.frame(x = x.major, y = y.range[1]) :
#     arguments imply differing number of rows: 0, 1

# Usando as facetas.

# Junta a informação.
br_data_dec <- inner_join(br_data,
                          filter(tb, ano %in% c(1980, 1991, 2000, 2010)))

ggplot(data = br_data_dec,
       mapping = aes(map_id = region)) +
    facet_wrap(facets = ~ano) +
    geom_map(mapping = aes(fill = prop_urb),
             map = br_map,
             colour = "black",
             size = 0.25) +
    scale_fill_distiller(palette = 4, direction = 1) +
    expand_limits(x = br_map$long, y = br_map$lat) +
    coord_map()

#-----------------------------------------------------------------------
# Mapa do Mato Grosso do Sul.

# Baseado em:
# http://eduardogutierres.com/inteligencia-geografica-gerando-mapas-em-r/

# Baixar arquivo ZIP deste endereço.
# https://www.dropbox.com/s/gngl79c8gpiwf5r/municipios_2010.zip

# Descompacta.
unzip("../data/municipios_2010.zip", exdir = "../data/")

# NOTE: tem o polígono de todos os municípios do Brasil.
# Lê o shape file.
# mapa <- readShapeSpatial("../data/Shapefiles/municipios_2010.shp")
mapa <- readOGR("../data/Shapefiles", use_iconv = TRUE, encoding = "UTF-8")
summary(mapa)

# Tabela de dados de cada polígono.
mapa_data <- as_tibble(mapa@data)
class(mapa_data)
str(mapa_data)

# Converte para o tipo de valor mais apropriado.
mapa_data <- mapa_data %>%
    mutate_at(vars(nome, populacao:codigo_ibg), as.character) %>%
    mutate_at(vars(populacao:codigo_ibg), as.integer)

# Tabela de coordenada de cada polígono.
mapa_poly <- as_tibble(tidy(mapa, region = "id"))
class(mapa_poly)
str(mapa_poly)

# Filtra para informações do MS.
mapa_data_ms <- mapa_data %>%
    filter(uf == "MS")
str(mapa_data_ms)

mapa_poly_ms <- mapa_poly %>%
    filter(is.element(id, unique(mapa_data_ms$id)))
str(mapa_poly_ms)

# Conversão para inteiro.
mapa_poly_ms$id <- as.numeric(mapa_poly_ms$id)

# Gráfico com mapeando a população na cor de preenchimento.
ggplot(data = mapa_data_ms,
       aes(map_id = id)) +
    geom_map(map = mapa_poly_ms,
             mapping = aes(fill = log10(populacao),
                           group = id),
             color = "black",
             size = 0.25) +
    expand_limits(x = mapa_poly_ms$long,
                  y = mapa_poly_ms$lat) +
    scale_fill_distiller(palette = "Reds", direction = 1) +
    coord_map() +
    theme_light()

#=======================================================================
# Usando arquivos KML.

#-----------------------------------------------------------------------
# Bairros de Curitiba.

# KML foi gerado a partir de arquivos shape.
kml <- "http://leg.ufpr.br/~walmes/data/bairros-cwb.kml"
cwb <- geojson_read(kml, method = "local", what = "sp")
class(cwb)

cwb_data <- as_tibble(cwb@data)
str(cwb_data)

cwb_poly <- as_tibble(tidy(cwb)) %>%
    rename(region = id)

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

# Adicionar a coordenada das coberturas à venda.
tb <- read_tsv("http://leg.ufpr.br/~walmes/data/coberturas-venda-cwb-26Jan2018.txt",
               skip = 5,
               quote = "")

ggplot(data = tb,
       mapping = aes(x = lon, y = lat)) +
    geom_point()

gg_cwb +
    geom_point(data = tb,
               mapping = aes(x = lon, y = lat, map_id = NA))

ggplot(data = cwb_poly,
       mapping = aes(x = long,
                     y = lat,
                     map_id = region)) +
           geom_map(map = cwb_poly,
                    alpha = 0.3,
                    colour = "white",
                    fill = "orange",
                    size = 0.25) +
           theme_light() +
           coord_map() +
    geom_point(data = cbind(tb, region = NA),
               mapping = aes(x = lon, y = lat),
               pch = 1, alpha = 0.5)

#-----------------------------------------------------------------------
# Estados do Brasil.

# NOTE: Baixar os KML dos site: http://www.gmapas.com/poligonos-ibge.

# ATTENTION: tem que usar `geom_map()` e não `geom_polygon()` para que
# as ilhas não fiquem ligadas com as partes continentais. A primeira
# assume que os dados estão tidy. A segunda não exige isso então fazer o
# gráfico é mais direto, porém, para mapear uma variável no
# preenchimento é mais fácil usar `geom_map` como já demonstrado.

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
  theme_light() +
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
