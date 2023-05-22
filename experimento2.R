# Experimento 2

library(lubridate)

table <- read.csv("table.csv") %>%
  as_tibble() %>%
  mutate(Y = C + G + X + I - M) %>%
  mutate(data = ymd(`X.1`)) %>%
  select(-`X.1`)

data_diff_log <-
  table %>%
  mutate(across(where(is.character), as.numeric)) %>%
  select(-data) %>%
  as.data.frame()


# Ajustar modelo
eq1 <- C ~ Y
eq2 <- I ~ 1
eq3 <- X ~ US + CN + 0
eq4 <- M ~ C + I
eq5 <- Y ~ C + I + G + X - M
eq6 <- G ~ Y

model <- systemfit(list(eq1, eq2, eq3, eq4, eq5, eq6), data = data_diff_log, method = "SUR")
summary(model)

# Fazer previsões para os próximos 12 períodos
pred <- predict(model, newdata = data.frame(G = rep(0,11*ncol(data))), interval = "prediction")

# LAVAAN ------------------------------------------------------------------

table2 <-
  table %>%
  select(-data) %>%
  mutate(CN=CN/1000000) %>%
  log() %>%
 # diff() %>%
  mutate(M = -M)

library(lavaan)

model <- '
  # latent variable definitions
  #  Y =~ C + I + G + X + M
  # regressions
    C ~ Y
    X ~ US + CN
    M ~ C + I
    G ~ Y
  # residual (co)variances

'

fit <- sem(model,
           data = table2)

coef(fit)
summary(fit)
varTable(fit)

predict(fit)

