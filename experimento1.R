library(httr)
library(jsonlite)
library(systemfit)
library(purrr)
library(stringr)

# Definir as séries temporais e o período de análise
series <- c("BCB4385", "BCB4374", "BCB4358", "BCB4364", "BCB4377", "BCB4380", "BCB4391", "BCB4370", "BCB4379", "BCB4404", "BCB4390")
start_date <- "01/01/2000"
end_date <- format(Sys.Date(), "%d/%m/%Y")

# Função para buscar dados no banco central usando a API
# Define a função para baixar dados do Banco Central

get_bcb_data <- function(series_list, start_date, end_date) {
  base_url <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.{}/dados?formato=json&dataInicial={}&dataFinal={}"

  url <- base_url %>%
    str_replace(., "\\{\\}", str_c(series_list, collapse = "%2C")) %>%
    str_replace(., "\\{\\}", start_date) %>%
    str_replace(., "\\{\\}", end_date)

  response <- GET(url)

  data <- response %>%
    content(as = "text") %>%
    fromJSON(flatten = TRUE) %>%
    as_tibble() %>%
    mutate(data = lubridate::dmy(data)) %>%
    spread(key = "serie", value = "valor")

  return(data)
}

# Baixar dados das séries
data_list <- map(series, get_bcb_data(series = series, start_date = start_date, end_date = end_date))
names(data_list) <- series

map(get_bcb_data(series = series, start_date = start_date, end_date = end_date))

# Unir dados em um único data.frame
data <- Reduce(function(...) merge(..., all = TRUE), data_list)
data <- data[complete.cases(data),]

# Renomear colunas
colnames(data) <- c("taxa_juros", "renda_disp", "consump", "export", "import", "renda_mundial", "preco_producao_interna", "tributos", "expect_invest", "expect_vendas", "taxa_cambio")

# Transformar os dados em log-diferenças
data_diff_log <- log(diff(data))

# Ajustar modelo
eq1 <- consump ~ beta1*renda_disp + beta2*tributos + beta3*expect_vendas
eq2 <- invest ~ alpha1*renda_disp + alpha2*taxa_juros + alpha3*expect_invest
eq3 <- export ~ gamma1*renda_mundial + gamma2*preco_producao_interna
eq4 <- import ~ delta1*renda_disp + delta2*taxa_cambio

model <- systemfit(list(eq1, eq2, eq3, eq4), data = data_diff_log, method = "SUR")

# Fazer previsões para os próximos 12 períodos
pred <- predict(model, newdata = data.frame(rep(0,11*ncol(data))), interval = "prediction")

# Transformar as previsões para a escala original
pred_orig <- exp(diffinv(pred$fit))
