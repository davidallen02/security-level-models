library(magrittr)

Rblpapi::blpConnect()

f <- function(tickers, field, type = 'Equity'){
  y <- tickers %>%
    stringr::str_to_upper() %>%
    paste(type) %>%
    Rblpapi::bdp(field) %>%
    dplyr::pull()
  
  return (y)
}

GrowthScore <- function(pam_rank,con_rank,dvd_rank,beta_rank){
  yy <- (pam_rank*.4) + (con_rank*.4) + (dvd_rank*.1) + (beta_rank*.1)
  
  return(yy)
}

ConservativeScore <- function(pam_rank,con_rank,dvd_rank,beta_rank){
  yy <- (pam_rank + con_rank + beta_rank)*.2 + (dvd_rank*.4) 
  
  return(yy)  
}

buylist <- readr::read_csv('buylist.csv', col_types = 'c') %>% dplyr::pull()

sectorWeights <- c('cond','cons','tels','rlst','inft','hlth','finl','matr','enrs','indu','util') %>%
  stringr::str_to_upper() %>%
  paste0('S5', .) %>%
  paste('Index') %>%
  Rblpapi::bdp(c('GICS_SECTOR_NAME','PERCENT_WEIGHT')) %>%
  dplyr::mutate(
    GICS_SECTOR_WEIGHT = PERCENT_WEIGHT,
    GICS_SECTOR_NAME = c('Consumer Discretionary',
                         'Consumer Staples',
                         'Communication Services',
                         'Real Estate',
                         'Information Technology',
                         'Health Care',
                         'Financials',
                         'Materials',
                         'Energy',
                         'Industrials',
                         'Utilities')
  ) %>%
  dplyr::select(GICS_SECTOR_WEIGHT, GICS_SECTOR_NAME)



dat <- readr::read_csv('./recommendations.txt', col_types = 'cDnnnnnl') %>%
  dplyr::filter(DATE <= Sys.Date()) %>%
  dplyr::arrange(desc(DATE)) %>%
  dplyr::distinct(TICKER, .keep_all = TRUE) %>%
  dplyr::filter(TICKER %in% buylist) %>%
  dplyr::filter(!SOURCE) %>%
  dplyr::mutate(
    PX_LAST            = TICKER %>% f('PX_LAST'),
    BEST_TARGET_PRICE  = TICKER %>% f('BEST_TARGET_PRICE'),
    EQY_DVD_YLD_IND    = TICKER %>% f('EQY_DVD_YLD_IND'),
    EQY_BETA           = TICKER %>% f('EQY_BETA'),
    GICS_SECTOR_NAME   = TICKER %>% f('GICS_SECTOR_NAME'),
  ) %>%
  dplyr::left_join(sectorWeights, by = 'GICS_SECTOR_NAME') %>%
  dplyr::group_by(GICS_SECTOR_NAME) %>%
  dplyr::mutate(
    PAM_UPSIDE            = PRICE_TARGET/PX_LAST,
    CONSENSUS_UPSIDE      = BEST_TARGET_PRICE/PX_LAST,
    PAM_UPSIDE_RANK       = PAM_UPSIDE %>% multiply_by(-1) %>% rank(),
    CONSENSUS_UPSIDE_RANK = CONSENSUS_UPSIDE %>% multiply_by(-1) %>% rank(),
    DVD_YLD_RANK          = EQY_DVD_YLD_IND %>% multiply_by(-1) %>% rank(),
    BETA_RANK             = EQY_BETA %>% multiply_by(-1) %>% rank(),
    GROWTH_SCORE          = GrowthScore(PAM_UPSIDE_RANK, 
                                        CONSENSUS_UPSIDE_RANK, 
                                        DVD_YLD_RANK, 
                                        BETA_RANK),
    CONSERVATIVE_SCORE    = ConservativeScore(PAM_UPSIDE_RANK, 
                                              CONSENSUS_UPSIDE_RANK, 
                                              DVD_YLD_RANK, 
                                              BETA_RANK),
    GROWTH_RANK       = GROWTH_SCORE %>% rank(),
    CONSERVATIVE_RANK = CONSERVATIVE_SCORE %>% rank(),
    TARGET_WEIGHT     = 5  %>% divide_by(GICS_SECTOR_WEIGHT) %>% multiply_by(100) %>% min(100),
    MAX_WEIGHT        = 10 %>% divide_by(GICS_SECTOR_WEIGHT) %>% multiply_by(100) %>% min(100)
    
  )

dat %>% readr::write_csv('full-data.csv')
dat %>% 
  dplyr::select(TICKER, GICS_SECTOR_NAME, GROWTH_RANK, 
                CONSERVATIVE_RANK, TARGET_WEIGHT, MAX_WEIGHT) %>%
  dplyr::arrange(GICS_SECTOR_NAME, GROWTH_RANK) %>%
    readr::write_csv('rankings-weights.csv')