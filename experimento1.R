library(httr)
library(jsonlite)
library(systemfit)
library(purrr)
library(stringr)
library(tidyr)
library(dplyr)

# Definir as séries temporais e o período de análise
series <- list("4385", "4374", "4358", "4364", "4377", "4380", "4391", "4370", "4379", "4404", "4390")
start_date <- "01/01/2010"
end_date <- format(Sys.Date(), "%d/%m/%Y")

# Função para buscar dados no banco central usando a API
# Define a função para baixar dados do Banco Central

get_bcb_data <- function(serie, start_date = "01/01/2000", end_date = format(Sys.Date(), "%d/%m/%Y")) {
  base_url <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.{}/dados?formato=json&dataInicial={}&dataFinal={}"

  url <- base_url %>%
    str_replace(., "\\{\\}", str_c(serie, collapse = "%2C")) %>%
    str_replace(., "\\{\\}", start_date) %>%
    str_replace(., "\\{\\}", end_date)

  response <- GET(url)

  data <- response %>%
    content(as = "text") %>%
    fromJSON(flatten = TRUE) %>%
    as_tibble() %>%
    mutate(data = lubridate::dmy(data),
           serie = serie)

  return(data)
}

# Baixar dados das séries
data_list <- map(series, get_bcb_data)
names(data_list) <- series

# Unir dados em um único data.frame
data <- Reduce(function(...) merge(..., all = TRUE), data_list)
data <- data[complete.cases(data),]
data <- data %>% pivot_wider(names_from = serie, values_from = valor) %>% drop_na()

# Renomear colunas
colnames(data) <- c("taxa_juros", "renda_disp", "consump", "export", "import",
                    "renda_mundial", "preco_producao_interna", "tributos",
                    "expect_invest", "expect_vendas", "taxa_cambio", "invest")

# Transformar os dados em log-diferenças
data_diff_log <-
  data %>%
  mutate(across(where(is.character), as.numeric))

# Ajustar modelo
eq1 <- consump ~ renda_disp + tributos + expect_vendas
eq2 <- invest ~ renda_disp + taxa_juros + expect_invest
eq3 <- export ~ renda_mundial + preco_producao_interna
eq4 <- import ~ renda_disp + taxa_cambio

model <- systemfit(list(eq1, eq2, eq3, eq4), data = data_diff_log, method = "SUR")

# Fazer previsões para os próximos 12 períodos
pred <- predict(model, newdata = data.frame(rep(0,11*ncol(data))), interval = "prediction")

# Transformar as previsões para a escala original
pred_orig <- exp(diffinv(pred$fit))



