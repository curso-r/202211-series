library(fable)
library(feasts)
library(timetk)
library(modeltime)
library(tidymodels)
library(tidyverse)

anac <- readr::read_rds("https://github.com/curso-r/main-series/blob/main/dados/anac-sp.rds?raw=true") %>%
  mutate(DATA_ym = tsibble::yearmonth(paste(ANO, MES, sep = "-"))) %>%
  mutate(
    TEMPO_DESDE_INICIO = difftime(
      DATA,
      lubridate::ymd("1999-12-01"),
      units = "days"
    )/30,
    LAG_1 = lag(PASSAGEIROS_PAGOS, 1, default = 0)
  )

# usando o timetk
anac %>%
  plot_time_series(DATA, PASSAGEIROS_PAGOS)

anac_ts <- anac %>%
  as_tsibble(index = DATA_ym) |>
  select(PASSAGEIROS_PAGOS, everything())

autoplot(anac_ts)

stl_components <- anac_ts %>%
  model(
    STL(PASSAGEIROS_PAGOS ~ season(12) + trend())
  ) %>%
  components()

autoplot(stl_components)

anac_ts |>
  ACF() |>
  autoplot()

#outra analise comum:

anac_ts |>
  ACF(PASSAGEIROS_PAGOS-LAG_1, lag_max = 48) |>
  autoplot()

# isso dá dica de que tem sazonalidade

# series com tendencia tem autocorrelacao que demora pra cair: ------------
serie_ingenua <- tibble(
  tempo = 1:100,
  y = tempo + rnorm(100, sd = 5)
)

serie_ingenua |>
  as_tsibble(index = tempo) |>
  gg_lag(geom = "point", lags = 1:24)


serie_ingenua |>
  as_tsibble(index = tempo) |>
  ACF(diff(y)) |>
  autoplot()

serie_ingenua |>
  as_tsibble(index = tempo) |>
  ACF() |>
  autoplot()

# fim do parenteses -------------------------------------------------------

stl_components %>%
  ACF(remainder) %>%
  autoplot()

gg_season(anac_ts)

# o que concluímos?

# existe tendência
# provavelmente existe alguma sazonalidade
# que 2020 provavelmente vai precisar de um tratamento especial

# Modelo

plot_time_series(anac, DATA, PASSAGEIROS_PAGOS)

split <- time_series_split(
  anac,
  DATA,
  initial = "20 years",
  assess = "12 month"
)

plot_time_series_cv_plan(
  tk_time_series_cv_plan(split),
  DATA, PASSAGEIROS_PAGOS)

# Definição do modelo inicial

model_spec <- parsnip::linear_reg() %>%
  set_engine("lm")

# AJuste do modelo - por enquanto não vamos usá-lo
modelo1 <- model_spec |>
  fit(PASSAGEIROS_PAGOS ~ ANO + as.factor(MES), training(split))

modelo2 <- model_spec |>
  fit(PASSAGEIROS_PAGOS ~ as.factor(MES), training(split))

modelo3 <- model_spec |>
  fit(PASSAGEIROS_PAGOS ~ ANO*I(DATA <= as.Date("2020-04-01")) + as.factor(MES), training(split))

modelo4 <- model_spec |>
  fit(PASSAGEIROS_PAGOS ~ CARGA_PAGA_KG + as.factor(MES), training(split))
# esse modelo não consegue fazer previsões! as cargas conhecidas até o momento
# são só as anteriores ao mes corrente

modelo5 <- model_spec |>
  fit(PASSAGEIROS_PAGOS ~ LAG_1 + as.factor(MES), training(split))
# esse moldeo só faz previsões de 1 mês a frente!

models_tbl <- modeltime_table(
  modelo1,
  modelo2,
  modelo3,
  modelo4,
  modelo5
)

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(split))

forecasts <- calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(split),
    actual_data = anac
  )

calibration_tbl %>%
  modeltime_accuracy()

plot_modeltime_forecast(forecasts)

modeltime_residuals(calibration_tbl) |>
  plot_modeltime_residuals()

modeltime_residuals(calibration_tbl) |>
  group_by(.model_id) |>
  plot_acf_diagnostics(.index, .residuals)
