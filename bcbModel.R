#load library
library(bimets)

#define the model
bcbModelDef <- "
MODEL

COMMENT> Curva de Phillips
BEHAVIORAL> ipca_livres
TSRANGE 2003 1 2017 4
EQ> ipca_livres = a1 + a2*TSLAG(ipca_livres, 1) + a3*TSLAG(ipca_livres, 2) +
  a4*Einf_1 + a5*TSLAG(hiato, 3) + a6*ic_l1 + a7*quarter
COEFF> a1 a2 a3 a4 a5 a6 a7

COMMENT> Curva IS
BEHAVIORAL> hiato
TSRANGE 2002 3 2017 4
EQ> hiato = b1 + b2*TSLAG(hiato, 1) + b3*juroreal_l1 + b4*dsup
COEFF> b1 b2 b3 b4

COMMENT> Regra de Taylor
BEHAVIORAL> selic
TSRANGE 2002 4 2017 4
EQ> selic = c1 + c2*TSLAG(selic, 1) + c3*TSLAG(selic, 2) + c4*desvio +
  c5*TSLAG(hiato, 1)
COEFF> c1 c2 c3 c4 c5

END
"
#load the model
#dados_reg = dados_reg[-1:-3,]

bcbModel <- LOAD_MODEL(modelText = bcbModelDef)

bcbModelData <- list(
  ipca_livres = TIMESERIES(dados_reg$ipca_livres,
                           START = c(2002,2), FREQ = 4),
  # ipca_l1 = TIMESERIES(dados_reg$ipca_l1,
  #                          START = c(2003,1), FREQ = 4),
  # ipca_l2 = TIMESERIES(dados_reg$ipca_l2,
  #                      START = c(2003,1), FREQ = 4),
  Einf_1 = TIMESERIES(dados_reg$Einf_1,
                       START = c(2002,2), FREQ = 4),
  # hiato_l3 = TIMESERIES(dados_reg$hiato_l3,
  #                      START = c(2003,1), FREQ = 4),
  ic_l1 = TIMESERIES(dados_reg$ic_l1[-1],
                        START = c(2002,3), FREQ = 4),
  quarter = TIMESERIES(as.numeric(dados_reg$quarter),
                       START = c(2002,2), FREQ = 4),
  hiato = TIMESERIES(dados_reg$hiato,
                       START = c(2002,2), FREQ = 4),
  # hiato_l1 = TIMESERIES(dados_reg$hiato_l1,
  #                      START = c(2003,1), FREQ = 4),
   juroreal_l1 = TIMESERIES(as.numeric(dados_reg$juroreal_l1[-1]),
                        START = c(2002,3), FREQ = 4),
  dsup = TIMESERIES(dados_reg$dsup,
                       START = c(2002,2), FREQ = 4),
  # ipca_l1 = TIMESERIES(dados_reg$ipca_l1,
  #                      START = c(2003,1), FREQ = 4),
  selic = TIMESERIES(as.numeric(dados_reg$selic),
                       START = c(2002,2), FREQ = 4),
  # selic_l1 = TIMESERIES(as.numeric(dados_reg$selic_l1),
  #                      START = c(2003,1), FREQ = 4),
  # selic_l2 = TIMESERIES(as.numeric(dados_reg$selic_l2),
  #                      START = c(2003,1), FREQ = 4),
  desvio = TIMESERIES(as.numeric(dados_reg$desvio),
                       START = c(2002,2), FREQ = 4)#,
  # hiato_l1 = TIMESERIES(dados_reg$hiato_l1,
  #                      START = c(2003,1), FREQ = 4)
)

#load time series into the model object
bcbModel <- LOAD_MODEL_DATA(bcbModel,bcbModelData)

bcbModel <- ESTIMATE(bcbModel)

bcbModel$modelData <- within(bcbModel$modelData,{

  ipca_livres  = TSEXTEND(bcbModelData$ipca_livres,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  # ipca_l1    = TSEXTEND(bcbModelData$ipca_l1,  UPTO=c(2020,4),EXTMODE='LINEAR')
  # ipca_l2    = TSEXTEND(bcbModelData$ipca_l2,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  Einf_1  = TSEXTEND(bcbModelData$Einf_1,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  # hiato_l3 = TSEXTEND(bcbModelData$hiato_l3,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  ic_l1 = TSEXTEND(bcbModelData$ic_l1,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  quarter = TSEXTEND(bcbModelData$quarter,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  hiato = TSEXTEND(bcbModelData$hiato,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  # hiato_l1 = TSEXTEND(bcbModelData$hiato_l1,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  juroreal_l1 = TSEXTEND(bcbModelData$juroreal_l1,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  dsup = TSEXTEND(bcbModelData$dsup,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  # ipca_l1 = TSEXTEND(bcbModelData$ipca_l1,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  selic = TSEXTEND(bcbModelData$selic,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  # selic_l1 = TSEXTEND(bcbModelData$selic_l1,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  # selic_l2 = TSEXTEND(bcbModelData$selic_l2,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  desvio = TSEXTEND(bcbModelData$desvio,  UPTO=c(2020,4),EXTMODE='CONSTANT')
  # hiato_l1 = TSEXTEND(bcbModelData$hiato_l1,  UPTO=c(2020,4),EXTMODE='CONSTANT')

})

bcbModel <- SIMULATE(bcbModel
                       ,simType='FORECAST'
                       ,TSRANGE=c(2018,1,2020,4)
                       ,simConvergence=0.00001
                       ,simIterLimit=100
                       ,quietly=TRUE
)

TABIT(bcbModel$simulation$selic)
TABIT(bcbModel$simulation$ipca_livres)
TABIT(bcbModel$simulation$hiato)
