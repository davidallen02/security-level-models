GetBLP <- function(tickers, fld, date){
  
  xx <- Rblpapi::bdh(
    securities = tickers,
    fields = fld,
    start.date = date - 100,
    end.date = date
  ) %>%
    dplyr::select(fld) %>%
    dplyr::pull() %>%
    tail(1)
  
  xx <- ifelse(length(xx) == 0, NA, xx)
  
  return(xx)
}