#' Preducted future city value
#'
#' @param data
#' @param city
#' @param year
#'
#' @return predicted value of city average
#' @export
#'
#' @examples
#' \dontrun{
#' predictCityavg(Cityavg, "San Antonio, TX", 2025)
#' }
predictCityavg <- function(data, city, year) {

  # Filter the data for the specified state
  cityn <- filter(data, RegionName == city) %>% select(-RegionName)
  cityLong <- pivot_longer(cityn, cols = everything(), names_to = "Year", values_to = "Avg_Price")
  cityLong$Year <- as.numeric(cityLong$Year)

  #create linear regression
  model <- lm(Avg_Price ~ Year, data = cityLong)

  #Create Prediction model
  pred <- predict(model, newdata = data.frame(Year = year))

  return(pred)
}
