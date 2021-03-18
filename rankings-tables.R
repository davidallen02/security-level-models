library(magrittr)


dat <- readr::read_csv(file = "slm.csv", col_types = c("ccnnc")) %>%
  tidyr::separate(col = `Model Name`, 
                  into = c("sector", "strategy"), 
                  sep = " - ") %>%
  dplyr::select(-`Legacy Position Flag`)
  
  
t <- dat %>%
  gt::gt()