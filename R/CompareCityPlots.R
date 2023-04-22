
#' Compare city averages
#'
#' @param data
#' @param a
#' @param b
#' @param c
#'
#' @return 3 plots side by side comparing 3 states of your choice
#' @export
#'
#' @examples
#' \dontrun{
#' CompareCityPlots(cityonly, "Houston, TX", "Austin, TX", "San Antonio, TX")
#' }
CompareCityPlots <- function(data, a, b, c) {
  a_data <- filter(data, RegionName == a) %>%
    select(-RegionName)

  b_data <- filter(data, RegionName == b) %>%
    select(-RegionName)

  c_data <- filter(data, RegionName == c) %>%
    select(-RegionName)

  a_long <- pivot_longer(a_data,
                         cols = everything(),
                         names_to = "Year",
                         values_to = "Avg_Price")

  b_long <- pivot_longer(b_data,
                         cols = everything(),
                         names_to = "Year",
                         values_to = "Avg_Price")

  c_long <- pivot_longer(c_data,
                         cols = everything(),
                         names_to = "Year",
                         values_to = "Avg_Price")

  a_plot <- ggplot(a_long, aes(x = Year, y = Avg_Price)) +
    geom_point(color = "hotpink") +
    xlab("Year") + ylab("Average Price") +
    ggtitle(paste0("Average House Prices in ", a)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  b_plot <- ggplot(b_long, aes(x = Year, y = Avg_Price)) +
    geom_point(color = "orange") +
    xlab("Year") + ylab("Average Price") +
    ggtitle(paste0("Average House Prices in ", b)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  c_plot <- ggplot(c_long, aes(x = Year, y = Avg_Price)) +
    geom_point(color = "black") +
    xlab("Year") + ylab("Average Price") +
    ggtitle(paste0("Average House Prices in ", c)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  plots <- grid.arrange(a_plot, b_plot, c_plot, ncol = 2, nrow = 2)

  return(plots)
}


#subset data to only include cities
#cityonly <- YearsAvg[, c(4, 6:29)]

