library(magrittr)

buylist <- readr::read_csv('buylist.csv', col_types = 'c') %>% 
  dplyr::pull() %>%
  stringr::str_to_upper()

# Read required columns of `Account Holdings (current).csv`
dat <- readr::read_csv(
  file = 'Account Holdings (current)  - safe.csv',
  col_types           = readr::cols_only(
    Symbol            = readr::col_character(),
    Classification    = readr::col_character(),
    `Asset Class`     = readr::col_character(),
    `Asset Sub Class` = readr::col_character(),
    Sector            = readr::col_character())) %>% 
  
  # Keep only common stock securities
  dplyr::filter(`Asset Class` == 'Common Stock') %>%
  
  # Keep only one observation of each security
  dplyr::distinct() %>%
  dplyr::select(Symbol, Sector) %>%
  
  # Remove buy list securities
  dplyr::mutate(buylist = Symbol %in% buylist) %>%
  dplyr::filter(!buylist) %>%
  dplyr::select(Symbol, Sector) %>%
  
  # Add applicable model names
  dplyr::mutate(
    GrowthModels  = Sector %>% stringr::str_to_upper() %>% paste('- Growth'),
    ConservModels = Sector %>% stringr::str_to_upper() %>% paste('- Conservative')
  ) %>%
  
  # Add rank, weight and legacy position dummy
  dplyr::mutate(
    'Symbol Weight'        = 0,
    'Symbol Rank'          = 0,
    'Legacy Position Flag' = 'Yes'
  ) %>%
  dplyr::select(-`Sector`) %>%
  
  # Make long data via reshape2::melt()
  reshape2::melt(
    id.vars = c('Symbol','Symbol Weight','Symbol Rank','Legacy Position Flag')
  ) %>%
  dplyr::mutate(`Model Name` = value) %>%
  dplyr::select('Model Name', 'Symbol', 'Symbol Weight','Symbol Rank','Legacy Position Flag')