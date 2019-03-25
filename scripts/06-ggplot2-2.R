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

library(tidyverse)

#-----------------------------------------------------------------------
# Futebol.

# Endereço do arquivo de dados.
url <- "http://leg.ufpr.br/~walmes/data/euro_football_players.txt"
browseURL(url)

# Importação.
tb <- read_tsv(file = url,
               col_names = TRUE,
               quote = "",
               comment = "#")
class(tb)
glimpse(tb)

#-----------------------------------------------------------------------
# Manipulação para responder as perguntas.

# Quantos jogadores por pais?
tb %>% count(country) %>% arrange(desc(n))

# Quantos jogadores por equipe?
tb %>% count(team) %>% arrange(desc(n))

# Quantos jogadores por posição?
tb %>% count(pos) %>% arrange(desc(n))

# O jogador mais velho de cada pais.
tb %>%
    group_by(country) %>%
    top_n(age, n = 1) %>%
    pull(name)

# O jogador mais alto de cada pais.
tb %>%
    group_by(country) %>%
    top_n(cm, n = 1)

# A média de idade dos jogadores em equipe.
# O total de gols por em equipe.
tb %>%
    group_by(team) %>%
    summarise(age_mean = mean(age, na.rm = TRUE),
              goal_sum = sum(goal, na.rm = TRUE)) %>%
    arrange(goal_sum)

# Considerando os todos os jogadores e aqueles que efetivamente jogaram.
tb %>%
    group_by(team) %>%
    summarise(n_team = n(),
              n_played = sum(!is.na(apps)),
              sum_goal = sum(goal, na.rm = TRUE),
              mean_goal_team = sum_goal/n_team,
              mean_goal_played = sum_goal/n_played)

#-----------------------------------------------------------------------
# Gráficos.

# Em média, jogadores mais novos fazem mais gol?
ggplot(data = tb,
       mapping = aes(x = age, y = goal)) +
    geom_point() +
    geom_smooth()

# Em média, jogadores mais leves fazem mais gol?
ggplot(data = tb,
       mapping = aes(x = kg, y = goal)) +
    geom_point() +
    geom_smooth()

# Em média, jogadores mais altos fazem mais faltas (varmelho + amarelo)?
ggplot(data = tb,
       mapping = aes(x = kg, y = goal)) +
    geom_point() +
    geom_smooth()

tb %>%
    mutate(age_c = cut(age, seq(16, 36, by = 2))) %>%
    group_by(age_c) %>%
    summarise(mean_goal = mean(goal, na.rm = TRUE))

tb2 <- tb %>%
    filter(!is.na(apps)) %>%
    replace_na(list(goal = 0,
                    red = 0,
                    yel = 0,
                    spg = 0))
glimpse(tb2)

tb$pos %>% unique() %>%
    str_replace(pattern = "^([A-Z]+)\\(?.*$", replacement = "\\1")

# tb$apps %>% unique() %>%
#     str_replace(pattern = "^([0-9]+)\\(?.*$", replacement = "\\1")
# tb$apps %>% unique() %>%
#     str_replace(pattern = "^.*\\(([0-9]+)\\)$", replacement = "\\1")

# Criar a posição em que joga.
# Criar o número de aparições.
tb2 <- tb2 %>%
    # distinct(apps) %>%
    separate(apps, into = c("tit", "res"), sep = "\\(") %>%
    mutate(tit = as.integer(tit),
           res = str_replace(res, "\\D", "") %>% as.integer()) %>%
    replace_na(list(res = 0)) %>%
    mutate(jogos = tit + res,
           position = str_replace(pos,
                                  pattern = "^([A-Z]+)\\(?.*$",
                                  replacement = "\\1"))
glimpse(tb2)

ggplot(tb2,
       mapping = aes(x = cm, y = kg)) +
    geom_point(alpha = 0.5) +
    # geom_jitter(alpha = 0.5) +
    # geom_hex() +
    # geom_bin2d() +
    # geom_density2d()
    # facet_wrap(facets = ~position) +
    geom_smooth(se = FALSE, method = "lm")

tb_aux <- tb2 %>%
    group_by(team, position) %>%
    summarise_at(c("yel", "red"), sum) %>%
    gather(key = "cartao",
           value = "tot",
           yel, red)
tb_aux

ggplot(data = tb_aux,
       mapping = aes(x = position,
                     y = tot,
                     fill = cartao)) +
    facet_wrap(facets = ~team) +
    geom_col(position = "dodge")

# Em média, jogadores mais leves acertam mais os chutes?
tb2 <- tb2 %>%
    mutate(acertos = goal/(spg * jogos))

ggplot(tb2,
       # mapping = aes(x = yel, color = position)) +
       # mapping = aes(x = acertos, color = position)) +
       mapping = aes(x = aw, color = position)) +
    stat_ecdf(geom = "step")
    # geom_density(alpha = 0.5)

ggplot(tb2,
       mapping = aes(x = cm,
                     y = aw)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(facets = ~position)

# Em média, jogadores mais velhos tem melhor passe de bola?
# Em média, jogadores mais altos ganham mais disputas aéreas?

#-----------------------------------------------------------------------
