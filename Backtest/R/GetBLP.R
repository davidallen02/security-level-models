GetBLP <- function(tickers, fld, date){
  
  xx <- Rblpapi::bdh(
    securities = tickers,
    fields = fld,
    start.date = date - 365,
    end.date = date
  )  %>%
    dplyr::select(fld) %>%
    set_colnames('X') %>%
    dplyr::filter(!is.na(X)) %>%
    dplyr::pull() %>%
    tail(1)
  
  xx <- ifelse(length(xx) == 0, NA, xx)
  
  return(xx)
}