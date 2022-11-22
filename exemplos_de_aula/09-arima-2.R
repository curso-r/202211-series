library(fable)
library(feasts)
library(tidyverse)
library(tsibble)

# Dados de exportação brasileira de soja obtidos do ComexBrasil.

soja_bruto <- readr::read_rds("https://github.com/curso-r/main-series/raw/main/dados/soja.rds")

A <- as.Date("1967-01-01")

tsibble::make_yearmonth(year = c(1970, 1970), month = c(1,2))

soja <- soja_bruto %>%
  mutate(DATA = tsibble::yearmonth(paste(CO_ANO, CO_MES)),
         DATA_jeito2 = tsibble::make_yearmonth(CO_ANO, CO_MES),
         DATA_normal = as.Date(DATA),
         MES = lubridate::month(DATA),
         TEMPO_DESDE_O_INICIO = DATA_normal-min(DATA_normal),
         log_KG_LIQUIDO = log(KG_LIQUIDO),
         dif_log_KG_LIQUIDO = c(0,diff(log_KG_LIQUIDO)))

soja_ts <- soja  %>%
  as_tsibble(index = DATA)

soja_ts_anual <- soja %>%
  group_by(CO_ANO) |>
  summarise(
    KG_LIQUIDO = sum(KG_LIQUIDO)
  ) %>%
  as_tsibble(index = CO_ANO)

autoplot(soja_ts, log(KG_LIQUIDO))

soja_ts |>
  gg_tsdisplay(log(KG_LIQUIDO), plot_type = "partial")

soja_ts |>
  gg_tsdisplay(dif_log_KG_LIQUIDO, plot_type = "partial")

split <- time_series_split(
  soja,
  DATA_normal,
  initial = "17 years",
  assess = "2 year"
)

plot_time_series_cv_plan(
  tk_time_series_cv_plan(split),
  DATA_normal, log_KG_LIQUIDO
)

# baseline linear
regressao_spec <- parsnip::linear_reg() |>
  set_engine("lm")

# arima
arima_spec <- modeltime::arima_reg() |>
  set_engine("auto_arima")

arima_escolhido <- modeltime::arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 1,
  non_seasonal_differences = 1,
  non_seasonal_ma = 0,
  seasonal_ar = 1,
  seasonal_differences = 1,
  seasonal_ma = 1
) |>
  set_engine("arima", include.drift = TRUE)

regressao <- regressao_spec |>
  fit(log_KG_LIQUIDO ~ DATA_normal + as.factor(CO_MES), training(split))

arima <- arima_spec |>
  fit(log_KG_LIQUIDO ~ DATA_normal, training(split))

arima_escolhido_fit <- arima_escolhido |>
  fit(log_KG_LIQUIDO ~ DATA_normal, training(split))


modelo_tbl <- modeltime_table(
  regressao,
  arima,
  arima_escolhido_fit
  #arima_2,
  #suavizacao
)


calibration_tbl <- modelo_tbl |>
  modeltime_calibrate(new_data = testing(split))

forecasts <- calibration_tbl |>
  modeltime_forecast(
    new_data = testing(split),
    actual_data = soja
  )

calibration_tbl |>
  modeltime_residuals()  |>
  filter(.model_id == 3) |>
  plot_acf_diagnostics(.index, .residuals)

plot_modeltime_forecast(forecasts)

calibration_tbl |>
  modeltime_accuracy() |>
  View()



