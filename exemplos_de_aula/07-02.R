library(fable)
library(feasts)
library(timetk)
library(modeltime)
library(tidymodels)
library(tidyverse)

anac <- readr::read_rds("https://github.com/curso-r/main-series/blob/main/dados/anac-sp.rds?raw=true") %>%
  mutate(DATA_ym = tsibble::yearmonth(paste(ANO, MES, sep = "-"))) %>%
  filter(DATA < lubridate::ymd("2019-01-01")) %>%
  mutate(
    TEMPO_DESDE_INICIO = difftime(
      DATA,
      lubridate::ymd("1999-12-01"),
      units = "days"
    )/30,
    LAG_1 = lag(PASSAGEIROS_PAGOS, 1, default = 0)
  )

anac_ts <- anac %>%
  as_tsibble(index = DATA_ym) |>
  select(PASSAGEIROS_PAGOS, everything())

autoplot(anac_ts)

anac_ts |>
  ACF(lag_max = 48) |>
  autoplot()

anac_ts |>
  PACF(lag_max = 36) |>
  autoplot()

anac_ts |>
  ACF(diff(PASSAGEIROS_PAGOS), lag_max = 36) |>
  autoplot()

anac_ts |>
  PACF(diff(PASSAGEIROS_PAGOS), lag_max = 36) |>
  autoplot()

anac_ts |>
  ACF(diff(diff(PASSAGEIROS_PAGOS)), lag_max = 36) |>
  autoplot()

anac_ts |>
  ACF(diff(PASSAGEIROS_PAGOS, lag = 12), lag_max = 36) |>
  autoplot()

anac_ts |>
  PACF(diff(PASSAGEIROS_PAGOS, lag = 12), lag_max = 36) |>
  autoplot()

anac_ts |>
  ACF(diff(PASSAGEIROS_PAGOS), lag_max = 36) |>
  autoplot()

