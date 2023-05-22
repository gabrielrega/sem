# Experimento 3

data <- read.csv("table.csv")

# BIBLIOTECAS
library(bimets)

# Definindo o modelo

ModelDef <- "
MODEL

COMMENT> Modelo ultra simplificado ICP-1
COMMENT> Gabriel Rega

COMMENT> Consumo
BEHAVIORAL> c
TSRANGE 1996 1 2022 4
EQ> c =  a1 + a2*y
COEFF> a1 a2

COMMENT> Investimento
BEHAVIORAL> i
TSRANGE 1996 1 2022 4
EQ> i = b1 + b2*c
COEFF> b1 b2

COMMENT> Governo
BEHAVIORAL> g
TSRANGE 1996 1 2022 4
EQ> g = c1 + c2*y
COEFF> c1 c2

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

COMMENT> Gross National Product
IDENTITY> y
EQ> y = c + i + g + x - m

END
"

# Carrega o modelo
Model <- LOAD_MODEL(modelText = ModelDef)

# Carrega dados
ModelData <- list(
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
Model <- LOAD_MODEL_DATA(Model,ModelData)
Model <- ESTIMATE(Model)

# Extendendo

Model$modelData <- within(Model$modelData,{
  us  = TSEXTEND(us,UPTO=c(2025,4),EXTMODE='LINEAR')
})

Model <- SIMULATE(Model
                  ,simType='FORECAST'
                  ,TSRANGE=c(2023,1,2025,4)
                  ,simConvergence=0.00001
                  ,simIterLimit=100
                  ,quietly=TRUE
)

TABIT(Model$simulation$y)
TABIT(Model$simulation$c)
TABIT(Model$simulation$i)
TABIT(Model$simulation$g)
TABIT(Model$simulation$x)
TABIT(Model$simulation$m)

forecast <-
data.frame(y = Model$simulation$y,
           c = Model$simulation$c,
           i = Model$simulation$i,
           g = Model$simulation$g,
           x = Model$simulation$x,
           m = Model$simulation$m)

readr::write_excel_csv2(forecast, "forecast.csv")
