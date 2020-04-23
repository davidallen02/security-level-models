library(magrittr)
readr::read_csv('./recommendations.txt', col_types = 'cDnnnnnl') %>%
  dplyr::filter(DATE <= Sys.Date()) %>%
  dplyr::arrange(desc(DATE)) %>%
  dplyr::distinct(TICKER, .keep_all = TRUE) %>%
  dplyr::filter(
    TICKER %in% (
      readr::read_csv('buylist.csv', col_types = 'c') %>% 
        dplyr::pull())) %>% View()