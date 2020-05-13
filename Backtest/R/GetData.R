source('./Backtest/R/DefineUniverse.R')
source('./Backtest/R/GetBLP.R')

library(magrittr)

GetData <- function(index, date){
  
  Rblpapi::blpConnect()
  
  index.full <- index %>% stringr::str_to_upper() %>% paste('Index')
  date1 <- date %>% stringr::str_remove_all('-')
  date2 <- date %>% as.Date()
  
  path <- './Backtest/Output/' %>% paste0(date1)
  
  if(!dir.exists(path)){dir.create(path)}
  
  tibble::tibble(
    tickers = DefineUniverse(index.full, date1) %>% paste('Equity')
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      PT = tickers %>% GetBLP('BEST_TARGET_PRICE', date2),
      PX = tickers %>% GetBLP('PX_LAST', date2),
      DY = tickers %>% GetBLP('DIVIDEND_INDICATED_YIELD', date2),
      BT = tickers %>% GetBLP('BETA_ADJ_OVERRIDABLE', date2),
      MC = tickers %>% GetBLP('CUR_MKT_CAP', date2)
    ) %>%
    readr::write_csv(
      path = path %>% paste0('/', index, '.csv')
    )
}


GetData('s5cond','2009-12-31') 
GetData('s5cons','2009-12-31')
GetData('s5tels','2009-12-31')
GetData('s5finl','2009-12-31')
GetData('s5inft','2009-12-31')
GetData('s5hlth','2009-12-31')
GetData('s5matr','2009-12-31')
GetData('s5enrs','2009-12-31')
GetData('s5util','2009-12-31')
GetData('s5indu','2009-12-31')

