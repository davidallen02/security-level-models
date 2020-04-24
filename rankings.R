library(magrittr)

f <- function(tickers, field){
  y <- tickers %>%
    stringr::str_to_upper() %>%
    paste('Equity') %>%
    Rblpapi::bdp(field) %>%
    dplyr::pull()
  
  return (y)
}

GrowthScore <- function(pam_rank,con_rank,dvd_rank,beta_rank){
  yy <- (pam_rank*.4) + (con_rank*.4) + (dvd_rank*.1) + (beta_rank*.1)
  
  return(yy)
}

ConservativeScore <- function(pam_rank,con_rank,dvd_rank,beta_rank){
  yy <- (pam_rank*.2) + (con_rank*.2) + (dvd_rank*.4) + (beta_rank*.2)
  
  return(yy)  
}

buylist <- readr::read_csv('buylist.csv', col_types = 'c') %>% dplyr::pull()

readr::read_csv('./recommendations.txt', col_types = 'cDnnnnnl') %>%
  dplyr::filter(DATE <= Sys.Date()) %>%
  dplyr::arrange(desc(DATE)) %>%
  dplyr::distinct(TICKER, .keep_all = TRUE) %>%
  dplyr::filter(TICKER %in% buylist) %>%
  dplyr::filter(!SOURCE) %>%
  dplyr::mutate(
    PX_LAST = TICKER %>% f('PX_LAST'),
    BEST_TARGET_PRICE = TICKER %>% f('BEST_TARGET_PRICE'),
    EQY_DVD_YLD_IND = TICKER %>% f('EQY_DVD_YLD_IND'),
    EQY_BETA = TICKER %>% f('EQY_BETA'),
    GICS_SECTOR_NAME = TICKER %>% f('GICS_SECTOR_NAME')
  ) %>%
  dplyr::group_by(GICS_SECTOR_NAME) %>%
  dplyr::mutate(
    PAM_UPSIDE = PRICE_TARGET/PX_LAST,
    CONSENSUS_UPSIDE = BEST_TARGET_PRICE/PX_LAST,
    PAM_UPSIDE_RANK = PAM_UPSIDE %>% multiply_by(-1) %>% rank(),
    CONSENSUS_UPSIDE_RANK = CONSENSUS_UPSIDE %>% multiply_by(-1) %>% rank(),
    DVD_YLD_RANK = EQY_DVD_YLD_IND %>% multiply_by(-1) %>% rank(),
    BETA_RANK = EQY_BETA %>% multiply_by(-1) %>% rank(),
    GROWTH_SCORE = GrowthScore(PAM_UPSIDE_RANK, 
                               CONSENSUS_UPSIDE_RANK, 
                               DVD_YLD_RANK, 
                               BETA_RANK),
    CONSERVATIVE_SCORE = ConservativeScore(PAM_UPSIDE_RANK, 
                                           CONSENSUS_UPSIDE_RANK, 
                                           DVD_YLD_RANK, 
                                           BETA_RANK),
    GROWTH_RANK = GROWTH_SCORE %>% rank(),
    CONSERVATIVE_RANK = CONSERVATIVE_SCORE %>% rank()
    
  ) %>% 
  readr::write_csv('rankings.csv')