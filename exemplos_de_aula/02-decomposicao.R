library(fable)
library(feasts)
library(tidyverse)
library(tsibble)

# Dados de exportação brasileira de soja obtidos do ComexBrasil.

soja <- readr::read_rds("https://github.com/curso-r/main-series/raw/main/dados/soja.rds")

A <- as.Date("1967-01-01")

tsibble::make_yearmonth(year = c(1970, 1970), month = c(1,2))

soja_ts <- soja %>%
  mutate(DATA = tsibble::yearmonth(paste(CO_ANO, CO_MES)),
         DATA_jeito2 = tsibble::make_yearmonth(CO_ANO, CO_MES)) %>%
  as_tsibble(index = DATA)

soja_ts_anual <- soja %>%
  group_by(CO_ANO) |>
  summarise(
    KG_LIQUIDO = sum(KG_LIQUIDO)
  ) %>%
  as_tsibble(index = CO_ANO)

autoplot(soja_ts, KG_LIQUIDO)

soja_ts |>
  gg_lag(y = KG_LIQUIDO, geom = "point")

soja_ts |>
  ACF(y = KG_LIQUIDO, lag_max = 60) |>
  autoplot()

soja_ts |>
  gg_season(y = KG_LIQUIDO)















# ACF de ums série crescente ----------------------------------------------

x <- 1:100 + rnorm(length(x), sd = 10)

serie_de_tempo_crescente = tsibble(
  tibble(X = x, tempo = 1:100), index = tempo
)

autoplot(serie_de_tempo_crescente)

ACF(serie_de_tempo_crescente) |>
  autoplot()

gg_lag(serie_de_tempo_crescente, geom = "point", lags = 1:24)


# Médias móveis -----------------------------------------------------------

x <- 1:100
x %>% slider::slide_dbl(
  .before = 7,
  .after = -1,
  mean,
  .complete = TRUE
)

mean(x[1:7])

soja_ts %>%
  mutate(
    # aqui escolhemos 6 antes, mas o ideal seria pegar a média considerando 12
    # meses
    MM = slider::slide_dbl(KG_LIQUIDO, .before = 5, .after = 6, mean, complete = TRUE)
  ) %>%
  autoplot(KG_LIQUIDO) +
  #ggplot(aes(x = DATA)) +
  geom_line(aes(y = MM), color = "red")


soja_ts %>%
  mutate(
    # aqui escolhemos 6 antes, mas o ideal seria pegar a média considerando 12
    # meses
    MM = slider::slide_dbl(KG_LIQUIDO, .before = 12, .after = -1, mean, complete = TRUE),
    KG_LIQUIDO_sem_tend = KG_LIQUIDO - MM
  ) |>
  #autoplot(KG_LIQUIDO_sem_tend) |>
  ACF(y = KG_LIQUIDO_sem_tend, lag_max = 50) |>
  autoplot()

soja_ts %>%
  mutate(
    # aqui escolhemos 5 antes e 6 depois e depois tiramos a média novamente
    # meses
    MM1 = KG_LIQUIDO %>%
      slider::slide_dbl(.before = 5, .after = 6, mean, .complete = TRUE),
    MM = MM1 %>%
      slider::slide_dbl(.before = 6, .after = 0, mean, complete = TRUE)
  ) %>%
  autoplot(KG_LIQUIDO) +
  geom_line(aes(y = MM), color = "red")


# Decomposição clássica ---------------------------------------------------

decomposicao <- soja_ts %>%
  mutate(
    # aqui escolhemos 5 antes e 6 depois e depois tiramos a média novamente
    # meses
    tendencia = KG_LIQUIDO %>%
      slider::slide_dbl(.before = 5, .after = 6, mean, complete = TRUE) %>%
      slider::slide_dbl(.before = 3, .after = 0, mean, complete = TRUE),
    estacionaria = KG_LIQUIDO - tendencia
  ) %>%
  #tibble::as_tibble() %>%
  group_by(CO_MES) %>%
  mutate(
    sazonalidade = mean(estacionaria, na.rm = TRUE),
    residuo = estacionaria - sazonalidade
  ) %>%
  ungroup()

decomposicao %>%
  select(DATA, KG_LIQUIDO, tendencia, sazonalidade, residuo) %>%
  pivot_longer(
    cols = c(KG_LIQUIDO, tendencia, sazonalidade, residuo),
    names_to = "serie",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = as.Date(DATA), y = valor)) +
  geom_line() +
  facet_wrap(ncol = 1, ~serie, scales = "free")

decomposicao |>
  ACF(y = residuo) |>
  autoplot()

# o feasts tem uma função para fazer isso automaticamente.

soja_ts %>%
  model(
    classical_decomposition(KG_LIQUIDO, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot()

# O que seria bom?

# Pouca autocorelação nos resíduos. Se ainda existe autocorrelação quer
# dizer que ainda existem componentes temporais que o nosso modelo não
# consegue capturar.

soja_ts %>%
  model(
    classical_decomposition(KG_LIQUIDO, type = "multiplicative")
  ) %>%
  components() %>%
  ACF(random) %>%
  autoplot()

# Seasonal-Trend Loess

# STL ajuda?
stl_components <- soja_ts %>%
  model(
    STL(KG_LIQUIDO ~ season(12) + trend())
  ) %>%
  components()

autoplot(stl_components)

stl_components %>%
  ACF(remainder) %>%
  autoplot()
