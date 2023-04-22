#' Predict future state averages
#'
#' @param data
#' @param state
#' @param year
#'
#' @return predicted value of state average
#' @export
#'
#' @examples
#' \dontrun{
#' predictStateavg(Stateavg, "TX", 2025)
#' }
predictStateavg <- function(data, state, year) {

  # Filter the data for the specified state
  state <- filter(data, StateName == state) %>% select(-StateName)
  state_long <- pivot_longer(state, cols = everything(), names_to = "Year", values_to = "Avg_Price")
  state_long$Year <- as.numeric(state_long$Year)

  #create linear regression
  model <- lm(Avg_Price ~ Year, data = state_long)

  #Create Prediction model
  pred <- predict(model, newdata = data.frame(Year = year))

  return(pred)
}
