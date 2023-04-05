# Reprodução da aula 10 da Análise Macro, Modelos do Banco Central.

library(devtools)
library(tidyverse)
library(readxl)
library(restriktor)
library(RcppRoll)
library(broom)
library(scales)
library(rbcb)
library(xts)
library(dynlm)
library(systemfit)

## Importar e Tratar Expectativas de Inflação
exp_ipca <- get_monthly_market_expectations("IPCA",
                                            end_date = "2018-09-31")
diff_quarter <- function(end_date, start_date){
  year_to_quarters <- (floor(end_date)*4 + (end_date %% 1)*10) -
    (floor(start_date)*4 + (start_date %% 1)*10)
  return(year_to_quarters)
}

exp_ipca_aux <- exp_ipca %>%
  dplyr::select(date, reference_date, median) %>%
  dplyr::mutate(reference_month = lubridate::ymd(paste(reference_date,
                                                       "01",
                                                       sep = "-"))) %>%
  dplyr::mutate(date_year = lubridate::year(date),
                date_month = lubridate::month(date)) %>%
  dplyr::group_by(date_year, date_month, reference_month) %>%
  dplyr::summarise(median_month = mean(median)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(date = lubridate::make_date(year = date_year,
                                            month = date_month)) %>%
  dplyr::select(date, reference_month, median_month) %>%
  dplyr::filter(date > "2001-12-01") %>%
  dplyr::mutate(ref_quarter = lubridate::quarter(reference_month,
                                                 with_year = T)) %>%
  dplyr::group_by(date, ref_quarter) %>%
  dplyr::summarise(median_quarter = last(((cumprod(1+(median_month/100)))-1)*100)) %>%
  dplyr::mutate(date_quarter = lubridate::quarter(date, with_year = T)) %>%
  dplyr::group_by(date_quarter, ref_quarter, .add = F) %>%
  dplyr::summarise(median_quarter = mean(median_quarter)) %>%
  dplyr::filter(ref_quarter > date_quarter) %>%
  dplyr::mutate(diff = round(diff_quarter(ref_quarter, date_quarter)),1) %>%
  dplyr::select(-ref_quarter) %>%
  tidyr::spread(key = diff, value = median_quarter)

colnames(exp_ipca_aux)[-1] <- paste("EInf_t+",
                                    colnames(exp_ipca_aux)[-1],
                                    sep = "")

acum_quarter <- function(x){

  x_fac <- 1+(x/100)
  x_cum <- RcppRoll::roll_prodr(x_fac, n = 3)
  x_qr <- last((x_cum-1)*100)
  return(x_qr)
}

dados_ipca <- read_csv2('am/ipca.csv',
                        col_types =
                          list(col_date(format='%d/%m/%Y'),
                               col_double(),
                               col_double())) %>%
  dplyr::mutate(date_quarter = lubridate::quarter(date, with_year = TRUE)) %>%
  dplyr::group_by(date_quarter) %>%
  dplyr::summarise_at(vars(ipca_total, ipca_livres), funs(acum_quarter))

acum_ic <- function(x){
  x_diff <- log(x/first(x))*100
  x_acum <- last(x_diff)
  return(x_acum)
}

dados_ic <- read_csv2('am/ic.csv',
                      col_types =
                        list(col_date(format='%d/%m/%Y'),
                             col_double())) %>%
  dplyr::mutate(date_quarter = lubridate::quarter(date, with_year = TRUE)) %>%
  dplyr::group_by(date_quarter) %>%
  dplyr::summarise_at(vars(ic_br), funs(acum_ic))

hiato <- read_excel("am/hiato.xlsx") %>%
  dplyr::mutate(date_quarter = as.numeric(gsub(" T", ".", date_qr)),
                hiato = hiato*100) %>%
  dplyr::select(date_quarter, hiato)

sup <- read_excel("am/sup.xlsx") %>%
  dplyr::mutate(date_quarter = as.numeric(gsub(" T", ".", date_qr)),
                sup = sup*100) %>%
  dplyr::select(date_quarter, sup)

dsup = diff(sup$sup)
dsup = tibble(date_quarter=sup$date_quarter[-1], dsup=dsup)

juro = read_excel('am/juro.xlsx',
                  col_types = c('date', rep('numeric',2)))

juroreal = (((1+(juro$swap/100))/(1+(juro$expinf/100)))-1)*100
juroreal = xts(juroreal, order.by=juro$date)
juroreal = apply.quarterly(juroreal, FUN=mean)
juroreal = tibble(date_quarter=sup$date_quarter, juroreal=juroreal)

selic <- read_excel("am/selic.xlsx")

selic = xts(selic$selic, order.by = selic$date)
selic = apply.quarterly(selic, FUN=mean)
selic = window(selic, start='2002-03-01', end='2017-12-01')
selic = tibble(date_quarter=sup$date_quarter, selic=selic)

## Importar e Tratar Expectativas de Inflação
exp_ipca <- get_twelve_months_inflation_expectations("IPCA",
                                                     end_date = "2017-12-31")

exp_ipca_d = xts(exp_ipca$mean, order.by = exp_ipca$date)
exp_ipca_q = apply.quarterly(exp_ipca_d, FUN=mean)
meta_ahead <- read_excel("am/meta_ahead.xlsx")
meta_ahead = xts(meta_ahead$meta_ahead, order.by = meta_ahead$date)
meta_ahead = apply.quarterly(meta_ahead, FUN=mean)
exp_ipca_q = ts(exp_ipca_q, start=c(2001,4), freq=4)
meta_ahead = ts(meta_ahead, start=c(2000, 1), freq=4)
desvio = exp_ipca_q - meta_ahead
desvio = window(desvio, start=c(2002,01))
desvio = tibble(date_quarter=sup$date_quarter, desvio=desvio)

swap <- read_excel("am/swap.xlsx")
swap = xts(swap$swap, order.by = swap$date)
swap = apply.quarterly(swap, FUN=mean)

## Importar e Tratar Expectativas para Selic

exp_selic <- get_monthly_market_expectations("Meta para taxa over-selic",
                                             end_date = "2018-09-31")

exp_selic <- get_market_expectations("selic", "Selic",
                                             end_date = "2018-09-31")

diff_quarter <- function(end_date, start_date){
  year_to_quarters <- (floor(end_date)*4 + (end_date %% 1)*10) -
    (floor(start_date)*4 + (start_date %% 1)*10)
  return(year_to_quarters)
}

exp_selic_aux <- exp_selic %>%
  dplyr::select(Data, reference_month, median) %>%
  dplyr::mutate(reference_month = lubridate::ymd(paste(reference_month,
                                                       "01",
                                                       sep = "-"))) %>%
  dplyr::mutate(date_year = lubridate::year(Data),
                date_month = lubridate::month(Data)) %>%
  dplyr::group_by(date_year, date_month, reference_month) %>%
  dplyr::summarise(median_month = mean(median)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(date = lubridate::make_date(year = date_year,
                                            month = date_month)) %>%
  dplyr::select(date, reference_month, median_month) %>%
  dplyr::filter(date > "2001-12-01") %>%
  dplyr::mutate(ref_quarter = lubridate::quarter(reference_month,
                                                 with_year = T)) %>%
  dplyr::group_by(date, ref_quarter) %>%
  dplyr::summarise(median_quarter = mean(median_month)) %>%
  dplyr::mutate(date_quarter = lubridate::quarter(date, with_year = T)) %>%
  dplyr::group_by(date_quarter, ref_quarter, add = F) %>%
  dplyr::summarise(median_quarter = mean(median_quarter)) %>%
  dplyr::filter(ref_quarter > date_quarter) %>%
  dplyr::mutate(diff = round(diff_quarter(ref_quarter, date_quarter)),1) %>%
  dplyr::select(-ref_quarter) %>%
  tidyr::spread(key = diff, value = median_quarter)

colnames(exp_selic_aux)[-1] <- paste("ESelic_t+",
                                     colnames(exp_selic_aux)[-1],
                                     sep = "")

swap = ts(swap, start=c(1999,3), freq=4)
selice = ts(exp_selic_aux$`ESelic_t+4`, start=c(2002,01), freq=4)
data = ts.intersect(swap, selice)
premio = data[,1]-data[,2]
premio = window(premio, end=c(2017,4))
premio = tibble(date_quarter=sup$date_quarter, premio=premio)

embi <- read_excel("am/embi.xlsx")
embi = xts(embi$embi, order.by = embi$date)
embi = embi[complete.cases(embi)]
embi = apply.quarterly(embi, FUN=mean)
risco_pais = ts(embi, start=c(1994,2), freq=4)
risco_pais = window(risco_pais, start=c(2002,01), end=c(2017,04))
risco_pais = tibble(date_quarter=sup$date_quarter,
                    risco_pais=risco_pais)


dados_reg <- dplyr::inner_join(dados_ipca, dados_ic) %>%
  dplyr::inner_join(hiato) %>%
  dplyr::inner_join(exp_ipca_aux) %>%
  dplyr::inner_join(juroreal) %>%
  dplyr::inner_join(dsup) %>%
  dplyr::inner_join(selic) %>%
  dplyr::inner_join(desvio) %>%
  #dplyr::inner_join(premio) %>%
  dplyr::inner_join(risco_pais) %>%
  dplyr::mutate(quarter = sub('.*\\.', '', date_quarter)) %>%
  dplyr::filter(date_quarter >= 2002.1) %>%
  dplyr::mutate(ipca_l1 = lag(ipca_total, 1),
                ipca_l2 = lag(ipca_total, 2),
                ipca_l3 = lag(ipca_total, 3),
                hiato_l1 = lag(hiato,1),
                hiato_l2 = lag(hiato,2),
                hiato_l3 = lag(hiato, 3),
                ic_l1 = lag(ic_br,1),
                ic_l2 = lag(ic_br,2),
                Einf_1 = `EInf_t+1`,
                juroreal_l1 = lag(juroreal,1),
                juroreal_l2 = lag(juroreal,2),
                selic_l1 = lag(selic,1),
                selic_l2 = lag(selic,2),
                selic_l3 = lag(selic,3) #,
                #premio_l1 = lag(premio,1),
                #premio_l2 = lag(premio,2),
                #risco_pais_l1 = stats::lag(risco_pais,k=1)
                )

cp = ipca_livres ~ ipca_l1 + ipca_l2 + Einf_1 + hiato_l3 + ic_l1 +
  quarter
is = hiato ~ hiato_l1 + juroreal_l1 + dsup
taylor = selic ~ selic_l1 + selic_l2 + desvio + hiato_l1
premio_swap = premio ~ premio_l1 + risco_pais
inst = ~ipca_l1 + ipca_l2 + ipca_l3 + Einf_1 + hiato_l2 + ic_l2 +
  quarter + hiato_l1 + juroreal_l2 + dsup + selic_l1 + selic_l2 +
  selic_l3 + desvio + premio_l2 + risco_pais_l1
system = list(cp=cp, is=is, taylor=taylor#, premioswap=premio_swap
              )

modelo_ols = systemfit(system, data=dados_reg)
round(coef(summary(modelo_ols)), 3)

# TSLS estimation
modelo_tsls = systemfit(system, method='2SLS', inst=inst,
                        data=dados_reg)
round(coef(summary(modelo_tsls)), 3)

