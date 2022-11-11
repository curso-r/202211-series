library(tidyverse)
library(tsibble)

# Ler uma tabela normal ---------------------------------------------------

tabela_poluicao_maior <- readr::read_rds("https://github.com/curso-r/main-series/raw/main/dados/cetesb_pinheiros_diario.rds")

tabela_poluicao <- readr::read_rds("https://raw.github.com/curso-r/main-series/main/dados/cetesb_pinheiros_diario_co.rds")

class(tabela_poluicao)

plot(tabela_poluicao)

tabela_poluicao_ts <- as_tsibble(
  tabela_poluicao,
  index = data
)

tabela_poluicao_mais_poluentes_ts <- as_tsibble(
  tabela_poluicao_maior,
  index = data,
  key = poluente
)

autoplot(tabela_poluicao_mais_poluentes_ts)


tabela_poluicao_ts |>
  gg_season()

tabela_poluicao
tabela_poluicao_ts
# aqui tem mudanças!

# O que muda? -------------------------------------------------------------

plot(tabela_poluicao)
plot(tabela_poluicao_ts)
# isso aqui não muda

library(feasts)

autoplot(tabela_poluicao_ts)
autoplot(tabela_poluicao)
# o autoplot vem do feasts e está preparado para mexer só com tsibble

tabela_poluicao_ts |>
  gg_lag(lags = 1, y = concentracao, geom = "point")
# dá certo!

tabela_poluicao |>
  gg_lag(lags = 1, y = concentracao, geom = "point")
# dá erro, pra fazer o lag a gente precisa de uma estrutura de série temporal
# logo tem que tsibble

# vamos tentar fazer o mesmo gráfico só que sem o gg_lag:

tabela_poluicao |>
  arrange(data) |>
  mutate(
    lag_concentracao = lag(concentracao),
    lag_concentracao_2 = lag(concentracao, n = 2)
  ) |>
  ggplot(aes(x = lag_concentracao, y = concentracao)) +
  geom_point()
# aqui precisei escrever bastante!
# e tomar o cuidado de fazer o arrange


# o legal do gg_lag é que eu posso fazer quantos lags eu quiser de maneira simples:

tabela_poluicao_ts |>
  gg_lag(lags = 1:30, y = concentracao, geom = "point")

tabela_com_lags <- tabela_poluicao |>
  arrange(data) |>
  mutate(
    lag_concentracao = lag(concentracao),
    lag_concentracao_2 = lag(concentracao, n = 2),
    lag_concentracao_3 =lag(concentracao, n = 3)
  ) |>
  drop_na()

cor(tabela_com_lags$concentracao, tabela_com_lags$lag_concentracao)

cor(tabela_com_lags$concentracao, tabela_com_lags$lag_concentracao_2)

cor(tabela_com_lags$concentracao, tabela_com_lags$lag_concentracao_3)

# Função de autocorrelação ------------------------------------------------

# é praticamente o gg_lag só que ao invés de vários gráficos, você substituir
# cada gráfico pela correlação entre X_t e lag(X_t) correspondente

# a função ACF constrói a função de autocorrelação

# Auto Correlation Function

tabela_poluicao_ts |>
  ACF(y = concentracao) |>
  autoplot()

# Sazonalidade ------------------------------------------------------------

tabela_poluicao_ts |>
  gg_season()

tabela_poluicao_ts |>
  gg_season(period = 10)

# visualizando outra série ------------------------------------------------


soja <- readr::read_rds("https://github.com/curso-r/main-series/raw/main/dados/soja.rds")

soja_ts <- soja |>
  mutate(
    data = tsibble::yearmonth(paste0(CO_ANO, "-", CO_MES))
  ) |>
  as_tsibble(
    index = data
  )

autoplot(soja_ts, .var = KG_LIQUIDO)

soja_ts |>
  gg_season(y = KG_LIQUIDO)

soja_ts |>
  ACF(y = KG_LIQUIDO, lag_max = 36) |>
  autoplot()

soja_ts |>
  gg_lag(lag = 1:24, y = KG_LIQUIDO)


# O que esperar de uma série temporal fraca -------------------------------

# Ou um conjunto de dados que não vale a pena olhar
# como série temporal

arrests_ts <- USArrests |>
  mutate(
    data = Sys.Date() + 1:n()
  ) |>
  as_tsibble(
    index = data
  )

arrests_ts |>
  ACF(y = Murder) |>
  autoplot()

arrests_ts |>
  gg_season(y = Murder, period = 7)

arrests_ts |>
  gg_lag(y = Murder, lags = 1:9)

# essa série se parece, no que diz respeito a séries temporais,
# com uma série do que se chama de ruído branco.

# o ruído branco é o diametralmente oposto da série temporal
# é o dado longitudinal mais puro de todos.

# O que a gente ruído branco? ---------------------------------------------

# resíduos, erros de regressão, de machine learning etc


# Analisando várias séries ------------------------------------------------

autoplot(tabela_poluicao_mais_poluentes_ts)

tabela_poluicao_mais_poluentes_ts |>
  ACF(y = concentracao) |>
  autoplot()



