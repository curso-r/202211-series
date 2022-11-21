# Suavização exponencial simples
library(tidymodels)

soja <- readr::read_rds("https://github.com/curso-r/main-series/raw/main/dados/soja.rds")

# Suavização exponencial simples

model <- forecast::ses(soja$KG_LIQUIDO/1000, initial = "simple")
model$model$par

sigmoid <- function(x) {
  1/(1 + exp(-x))
}

suav_exp <- function(y) {
  function(pars) {
    alpha <- sigmoid(pars[1])
    l_0 <- pars[2]
    y_hat <- numeric(length = length(y))
    y_hat[1] <- l_0
    for (i in 2:length(y)) {
      y_hat[i] <- alpha*y[i-1] + (1-alpha)*y_hat[i-1]
    }
    #mean((y-y_hat)^2)
    y_hat
  }
}

y <- soja$KG_LIQUIDO/1000
opt <- optim(fn = suav_exp(y), par = c(0, y[1]))
sigmoid(opt$par[1])
opt$par[2]

y <- filter(anac, ANO <= 2019)$PASSAGEIROS_PAGOS
opt <- optim(fn = suav_exp(y), par = c(0, y[1]))
sigmoid(opt$par[1])
opt$par[2]

model <- forecast::ses(filter(anac, ANO <= 2019)$PASSAGEIROS_PAGOS, initial = "simple")
model$model$par

# Holt

model <- forecast::holt(soja$KG_LIQUIDO/1000, initial = "simple")
plot(model)

model$model$par

holt <- function(y) {
  function(pars) {
    alpha <- sigmoid(pars[1])
    beta <- sigmoid(pars[2])
    l_0 <- pars[3]
    b_0 <- pars[4]
    y_hat <- numeric(length = length(y))
    y_hat[1] <- l_0 + b_0

    l_anterior <- l_0
    b_anterior <- b_0
    for (i in 2:length(y)) {
      l <- alpha*y[i-1] + (1-alpha)*(l_anterior + b_anterior)
      b <- beta*(l - l_anterior) + (1-beta)*b_anterior

      y_hat[i] <- l + b

      l_anterior <- l
      b_anterior <- b
    }
    mean((y-y_hat)^2)
  }
}

y <- soja$KG_LIQUIDO/1000
opt <- optim(
  fn = holt(y),
  par = c(inv_sigmoid(0.3), inv_sigmoid(0.1), y[1], y[2] - y[1])
)

sigmoid(opt$par[1])
sigmoid(opt$par[2])
opt$par[3]
opt$par[4]

model <- forecast::hw(ts(soja$KG_LIQUIDO/1000,frequency = 12), initial = "simple")
plot(model)
