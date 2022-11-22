library(fable)
library(feasts)
library(timetk)
library(modeltime)
library(tidymodels)
library(tsibble)
library(tidyverse)

anac <- readr::read_rds("https://github.com/curso-r/main-series/blob/main/dados/anac-sp.rds?raw=true") %>%
  mutate(DATA_ym = tsibble::yearmonth(paste(ANO, MES, sep = "-"))) %>%
  mutate(
    TEMPO_DESDE_INICIO = difftime(
      DATA,
      lubridate::ymd("1999-12-01"),
      units = "days"
    )/30,
    LAG_1 = lag(PASSAGEIROS_PAGOS, 1, default = 0),
    CARGA_LAG = lag(CARGA_PAGA_KG),
    dif_CARGA_LAG = CARGA_LAG - lag(CARGA_LAG, 2)
  ) |>
  filter(DATA <= as.Date("2018-12-31"))

anac_ts <- anac |>
  as_tsibble(index = DATA_ym)

anac_ts |>
  gg_tsdisplay(PASSAGEIROS_PAGOS, plot_type = "partial",
               lag_max = 60)

anac_ts |>
  gg_tsdisplay(
    difference(PASSAGEIROS_PAGOS, 1),
    plot_type = "partial",
    lag_max = 60
  )

anac_ts |>
  gg_tsdisplay(
    difference(difference(PASSAGEIROS_PAGOS, 1), 12),
    plot_type = "partial",
    lag_max = 60
  )

split <- time_series_split(
  anac,
  DATA,
  initial = "17 years",
  assess = "1 year"
)

plot_time_series_cv_plan(
  tk_time_series_cv_plan(split),
  DATA, PASSAGEIROS_PAGOS
)

# baseline com sazonalidade
modelo_naive_com_sazonalidade <- modeltime::naive_reg(seasonal_period = "1 year") %>%
  set_engine("snaive")

# baseline linear
regressao_spec <- parsnip::linear_reg() |>
  set_engine("lm")

# arima
arima_spec <- modeltime::arima_reg() |>
  set_engine("auto_arima")

arima_spec_escolhido <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 1,
  non_seasonal_differences = 1,
  non_seasonal_ma = 2,
  seasonal_ar = 1,
  seasonal_differences = 1,
  seasonal_ma = 1) |>
  set_engine("arima")

# ultima aula
#ets_spec <- modeltime::seasonal_reg()

naive_sazonal <- modelo_naive_com_sazonalidade |>
  fit(PASSAGEIROS_PAGOS ~ DATA +, training(split))

regressao <- regressao_spec |>
  fit(PASSAGEIROS_PAGOS ~ CARGA_LAG + TEMPO_DESDE_INICIO + as.factor(MES), training(split))

arima <- arima_spec |>
  fit(PASSAGEIROS_PAGOS ~ CARGA_LAG + as.numeric(TEMPO_DESDE_INICIO) + DATA, training(split))

arima_dif_carga <- arima_spec |>
  fit(PASSAGEIROS_PAGOS ~ dif_CARGA_LAG + as.numeric(TEMPO_DESDE_INICIO) + DATA, training(split))

arima_escolhido <- arima_spec_escolhido |>
  fit(PASSAGEIROS_PAGOS ~ CARGA_LAG + DATA, training(split))

# arima_2 <- arima_spec |>
#   fit(PASSAGEIROS_PAGOS ~ DATA, training(split))

# suavizacao <- ets_spec |>
#   fit(PASSAGEIROS_PAGOS ~ DATA, training(split))

modelo_tbl <- modeltime_table(
  regressao,
  arima,
  arima_dif_carga,
  arima_escolhido
  #arima_2,
  #suavizacao
)

calibration_tbl <- modelo_tbl |>
  modeltime_calibrate(new_data = testing(split))

forecasts <- calibration_tbl |>
  modeltime_forecast(
    new_data = testing(split),
    actual_data = anac
  )

calibration_tbl |>
  modeltime_residuals()  |>
  filter(.model_id == 1) |>
  plot_acf_diagnostics(.index, .residuals)

plot_modeltime_forecast(forecasts)

calibration_tbl |>
  modeltime_accuracy() |>
  View()
