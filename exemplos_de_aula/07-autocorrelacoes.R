# y é um numero fixo acrescido de um erro aleatorio -----------------------

set.seed(123)

y_fixo_com_ruido <- tsibble(
  mutate(tibble(x = 1:50), y = x+rnorm(50)),
  index = x)

y_fixo_com_ruido |>
  autoplot()

y_fixo_com_ruido |>
  ACF() |>
  autoplot()

y_fixo_com_ruido |>
  PACF(y) |>
  autoplot()

y_fixo_com_ruido |>
  ACF(diff(y)) |>
  autoplot()

y_fixo_com_ruido |>
  PACF(diff(y)) |>
  autoplot()

# que padrão é esse? esse é novo!

# série em que o Y é sempre o Y anterior + um errinho aleatório -----------

set.seed(123)

y_cresce_aleatoriamente <- tsibble(
  tibble(y = cumsum(rnorm(50, 1, 2)), x = 1:50),
  index = x)

y_cresce_aleatoriamente |>
  autoplot()

y_cresce_aleatoriamente |>
  ACF(y) |>
  autoplot()

y_cresce_aleatoriamente |>
  PACF(y) |>
  autoplot()

y_cresce_aleatoriamente |>
  gg_lag(geom = "point")

y_cresce_aleatoriamente |>
  ACF(diff(y)) |>
  autoplot()

# série em que o Y e o Y anterior X peso + erro elatório ------------------

#set.seed(123)

N <- 1000

erros <- rnorm(N,mean =  1)

y <- NULL
y[1] = 1

peso <- 0.5

for(i in 2:(N+1)){
  y[i] = peso*y[i-1]+erros[i-1]
}

y <- y[-1]

y_cresce_com_peso <- tsibble(
  tibble(
    y = y,
    x = 1:N),
  index = x)

y_cresce_com_peso |>
  autoplot()

y_cresce_com_peso |>
  ACF(y) |>
  autoplot()

y_cresce_com_peso |>
  ACF(diff(y)) |>
  autoplot()

# A situação em que tem peso é muito mais complicada...

# o peso faz com que diff(y) não seja só o errinho
# se o peso é negativo a aucorrelação oscila

y_cresce_com_peso |>
  PACF(y) |>
  autoplot()

# as correlações são os betas de regressões como essa:
y_cresce_com_peso |>
  mutate(y_lag = lag(y),
         y_lag_2 = lag(y, 2),
         y_lag_3 = lag(y, 3)) |>
  lm(y~y_lag+y_lag_2+y_lag_3, data = _)
