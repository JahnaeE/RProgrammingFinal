#' Clean Data
#'
#'
#' @param data
#'
#' @return new dataframe that is cleaned
#' @export
#'
#' @examples
#' \dontrun{
#' CleanedHousesAvg <- cleandata(HousesAvg)
#' }
#'
#get data
data_file <- "C:/Users/13212/OneDrive/Documents/College/USF/Spring 23/R Programming/Final/HomeValues/data/HousesAvg.csv"
HousesAvg <- read.csv(data_file)

cleandata <- function(data) {
  # Rename columns with date format
  new_col_names <- names(data)
  new_col_names[6:length(new_col_names)] <- gsub("X", "", new_col_names[6:length(new_col_names)])
  new_col_names[6:length(new_col_names)] <- gsub("\\.", "/", new_col_names[6:length(new_col_names)], fixed = TRUE)
  colnames(data) <- new_col_names

  # Convert date columns to date objects
  for (i in 6:length(data)) {
    if (is.character(data[[i]])) {
      data[[i]] <- as.Date(data[[i]], format = "%Y/%m/%d")
    }
  }

  # Remove NA values
  cleaneddata <- na.omit(data)

  return(cleaneddata)
}
