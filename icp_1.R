# Experimento 3 - MODELO ICP-1

# Dados do excel
data <- read.csv("table.csv")

# Bibliotecas
library(bimets)

# Definindo o modelo

def_modelo <- "
MODEL

COMMENT> Modelo ultra simplificado ICP-1
COMMENT> Gabriel Rega

COMMENT> Consumo
BEHAVIORAL> c
TSRANGE 1996 1 2022 4
EQ> c =  a1 + a2*y
COEFF> a1 a2

COMMENT> Exportacao
BEHAVIORAL> x
TSRANGE 1996 1 2022 4
EQ> x = d1 + d2*us
COEFF> d1 d2

COMMENT> Importacao
BEHAVIORAL> m
TSRANGE 1996 1 2022 4
EQ> m = e1 + e2*c + e3*i
COEFF> e1 e2 e3

COMMENT> Produto Interno Bruto
IDENTITY> y
EQ> y = c + i + g + x - m

END
"

# Carrega o modelo no bimets
modelo <- LOAD_MODEL(modelText = def_modelo)

# Carrega dados formatados para o bimets
dados_modelo <- list(
  c = TIMESERIES(data$C,
                  START=c(1996,1),FREQ=4),
  i = TIMESERIES(data$I,
                 START=c(1996,1),FREQ=4),
  g = TIMESERIES(data$G,
                 START=c(1996,1),FREQ=4),
  x = TIMESERIES(data$X,
                 START=c(1996,1),FREQ=4),
  m = TIMESERIES(data$M,
                 START=c(1996,1),FREQ=4),
  y = TIMESERIES(data$C+data$G+data$I+data$X-data$M,
                 START=c(1996,1),FREQ=4),
  cn = TIMESERIES(data$CN,
                 START=c(1996,1),FREQ=4),
  us = TIMESERIES(data$US,
                 START=c(1996,1),FREQ=4)
)

# Estimando o modelo
modelo <- LOAD_MODEL_DATA(modelo, dados_modelo)
modelo <- ESTIMATE(modelo)

# Fazendo a previsão

# Premissas
modelo$modelData <- within(modelo$modelData,{
  us  = TSEXTEND(us,UPTO=c(2025,4),EXTMODE='CONSTANT')
  g   = TSEXTEND(g ,UPTO=c(2025,4),EXTMODE='MYRATE', FACTOR=1.005)
  i   = TSEXTEND(i ,UPTO=c(2025,4),EXTMODE='MYRATE', FACTOR=1.005 )
})

# Simulação
modelo <- SIMULATE(modelo
                  ,simType='FORECAST'
                  ,TSRANGE=c(2023,1,2025,4)
                  ,simConvergence=0.00001
                  ,simIterLimit=100
                  ,quietly=TRUE
)

forecast <-
data.frame(y = modelo$simulation$y,
           c = modelo$simulation$c,
           i = tail(modelo$modelData$i,12),
           g = tail(modelo$modelData$g,12),
           x = modelo$simulation$x,
           m = modelo$simulation$m)

forecast

# Registro
readr::write_excel_csv2(forecast, "forecast.csv")
dados <- as.data.frame(do.call(cbind, modelo$modelData))
readr::write_excel_csv2(dados, "dados.csv")
