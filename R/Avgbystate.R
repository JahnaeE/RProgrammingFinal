#' Averages by state
#'
#' @param data
#'
#' @return dataset of states averages combined
#' @export
#'
#' @examples
#' \donrun{
#' Stateavg <- Avgbystate(YearsAvg)
#' }
library(dplyr)
Avgbystate <- function(data) {


  subsetyear <- data[, c(3, 6:29)]

  newsubset <- subsetyear %>%
    group_by(StateName) %>%
    summarise(across(c(2:24), mean))


  return(newsubset)
}
