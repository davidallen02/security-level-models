GetData <- function(index, date){
  tibble::tibble(
    tickers = DefineUniverse(index, date) %>% paste('Equity')
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      PT = tickers %>% GetBLP('BEST_TARGET_PRICE'),
      PX = tickers %>% GetBLP('PX_LAST'),
      DY = tickers %>% GetBLP('DIVIDEND_INDICATED_YIELD'),
      BT = tickers %>% GetBLP('BETA_ADJ_OVERRIDABLE'),
      MC = tickers %>% GetBLP('CUR_MKT_CAP')
    ) %>%
    readr::write_csv(
      path = './Backtest/Output/' %>% paste0(index, '.csv')
    )
}