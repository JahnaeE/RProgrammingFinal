#' Months Combined
#'
#' @param data
#'
#' @return dataset of the months combined
#' @export
#'
#' @examples
#' \dontrun{
#' YearsAvg <- MonthsCombined(CleanedHousesAvg)
#' }
MonthsCombined <- function(data) {
  # Subset the relevant columns
  values <- 6:ncol(data)

  # Initialize a new data frame to store the results
  yeardata <- data.frame( RegionID = data$RegionID,
                          SizeRank = data$SizeRank,
                          StateName = data$StateName,
                          RegionName = data$RegionName,
                          RegionType = data$RegionType)

  # Loop through each year in the dataset
  for (year in 2000:2023) {
    newcols <- grep(paste0(year, "\\."), colnames(data))
    yeardata[[as.character(year)]] <- rowMeans(data[, newcols])
  }

  return(yeardata)
}
