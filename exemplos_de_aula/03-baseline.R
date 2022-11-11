
# Carregar dados ----------------------------------------------------------

cetesb_pinheiros_co <- readr::read_rds("https://github.com/curso-r/main-series/raw/main/dados/cetesb_pinheiros_diario_co.rds")

# Intro timetk/modeltime --------------------------------------------------

library(timetk)
library(modeltime)

cetesb_pinheiros_co %>%
  plot_time_series(data, concentracao)

cetesb_pinheiros_co %>%
  plot_time_series_boxplot(data, concentracao, .period = "1 months")

cetesb_pinheiros_co %>%
  plot_seasonal_diagnostics(data, concentracao)

cetesb_pinheiros_co %>%
  plot_acf_diagnostics(data, concentracao)

# não funciona pois temos missing
cetesb_pinheiros_co %>%
  plot_stl_diagnostics(data, concentracao)

# Tidy --------------------------------------------------------------------

library(tidyverse)

# missing implícito...

cetesb_pinheiros_co_implicito <- cetesb_pinheiros_co %>%
  filter(!is.na(concentracao))

cetesb_pinheiros_co_implicito %>%
  plot_stl_diagnostics(data, concentracao)

sum(is.na(cetesb_pinheiros_co_implicito$concentracao))

# transforma missing implícitos em missing explícitos.
cetesb_pinheiros_co_implicito <- cetesb_pinheiros_co_implicito %>%
  timetk::pad_by_time(data, .by = "day")

sum(is.na(cetesb_pinheiros_co_implicito$concentracao))

cetesb_pinheiros_co_implicito %>%
  plot_time_series(data, concentracao)

# Essa base possui observações com missing
summary(cetesb_pinheiros_co$concentracao)

imputacoes <- cetesb_pinheiros_co %>%
  mutate(
    concentracao_linear = ts_impute_vec(concentracao, period = 1, lambda = NULL),
    # tem só um pouquinho de dataleak
    concentracao_boxcox = ts_impute_vec(concentracao, period = 1, lambda = 2),
    # tem só um pouquinho de dataleak
    concentracao_sazonal = ts_impute_vec(concentracao, period = 365),
    # tem dataleak
    concentracao_repetida = concentracao
  ) |>
  tidyr::fill(concentracao_repetida)
  # 0 dataleak

imputacoes %>%
  select(data, starts_with("concentracao")) %>%
  pivot_longer(
    cols = starts_with("concentracao"),
    names_to = "imputacao",
    values_to = "value"
  ) %>%
  plot_time_series(data, value, .facet_var = imputacao)

# inputacao final

cetesb_pinheiros_co <- cetesb_pinheiros_co %>%
  mutate(
    concentracao = ts_impute_vec(concentracao, period = 365)
  )


# Backtesting -------------------------------------------------------------

library(tidymodels)

split <- time_series_split(
  cetesb_pinheiros_co,
  data,
  initial = "40 months",
  assess = "1 month"
)

split %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(data, concentracao)

backtest <- time_series_cv(
  training(split),
  data,
  cumulative = FALSE,
  initial = "24 months",
  assess = "3 month",
  skip = "2 month",
  slice_limit = 10
)

split
backtest
plot_time_series_cv_plan(backtest, data, concentracao)

# Ajustando o modelo ------------------------------------------------------

library(tidymodels)
library(modeltime.resample)

# Definição do modelo inicial

# parecido com a sintaxe do parsnip/tidymodels
# linear_reg() %>%
#   set_mode("regression") %>%
#   set_engine("glmnet")

?modeltime::naive_reg

model_spec <- modeltime::naive_reg() %>%
  set_engine("naive")

# AJuste do modelo - por enquanto não vamos usá-lo
# lm(y ~ x1 + x2 + x3)
fitted <- model_spec %>%
  fit(concentracao ~ data, training(split))


modelo_naive_com_sazonalidade <- modeltime::naive_reg(seasonal_period = "1 year") %>%
  set_engine("snaive")

fitted2 <- modelo_naive_com_sazonalidade %>%
  fit(concentracao ~ data, training(split))

modelo_naive_com_sazonalidade_mensal <- modeltime::naive_reg(seasonal_period = "1 month") %>%
  set_engine("snaive")

fitted3 <- modelo_naive_com_sazonalidade_mensal %>%
  fit(concentracao ~ data, training(split))


# Colocar o modelo na model_tbl
models_tbl <- modeltime_table(
  fitted,
  fitted2,
  fitted3
)

# Re ajustar o modelo para as amostras do backtesting
resamples_fitted <- models_tbl  %>%
  modeltime_fit_resamples(
    resamples = backtest,
    control   = control_resamples(verbose = FALSE)
  )

# Visualizar os resultados
# metricas do pacote yardstick.
resamples_fitted %>%
  plot_modeltime_resamples(.metric_set = metric_set(rmse))

resamples_fitted %>%
  modeltime_resample_accuracy(
    summary_fns = mean,
    metric_set = metric_set(rmse)
  )

#....
# uma vez contentes com os modelos podemos gerar previsões p/ a base de teste
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(split))

forecasts <- calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(split),
    actual_data = cetesb_pinheiros_co
  )

calibration_tbl %>%
  modeltime_accuracy()

plot_modeltime_forecast(forecasts)

# Agora vamos refitar o modelo com a base toda

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = cetesb_pinheiros_co)

forecast_futuro <- refit_tbl %>%
  modeltime_forecast(h = "1 month", actual_data = cetesb_pinheiros_co)

forecast_futuro %>%
  plot_modeltime_forecast()

# revisando:

# vou modelar.

# escrever as especificações, fita e depois cria a modeltime_table().

# modeltime_table() |> modeltime_fit_resamples()

# pra ter a tabela de erros

# modeltime_table() |> modeltime_fit_resamples() |> modeltime_resample_accuracy()

# já na base final, após escolha de modelo:

# modeltime_table() |> modeltime_calibrate() |> modeltime_forecast()
