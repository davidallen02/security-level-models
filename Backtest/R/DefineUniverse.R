DefineUniverse <- function(index, date){
  
  universe <- Rblpapi::bds(
    security   = index, 
    field      = 'INDX_MWEIGHT_HIST',
    overrides  = c(
      'END_DATE_OVERRIDE' = date
      )
    ) %>%
    dplyr::select(`Index Member`) %>%
    dplyr::pull()
  
  return(universe)
}