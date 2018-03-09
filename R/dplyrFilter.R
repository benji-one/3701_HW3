#' Filters housing data by state
#'@param x Housing data
#'@param STATE State name two letter abbreviation
#'

filterState <- function(x,STATE)
{
 x %>% filter(State=="STATE")
}
