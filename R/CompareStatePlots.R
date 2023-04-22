library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

#' Compare state plots
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
#' predictStateavg(Stateavg, "TX", 2025)
#' }
CompareStatePlots <- function(data, a, b, c) {
  a_data <- filter(data, StateName == a) %>%
    select(-StateName)

  b_data <- filter(data, StateName == b) %>%
    select(-StateName)

  c_data <- filter(data, StateName == c) %>%
    select(-StateName)

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
    geom_point(color = "green") +
    xlab("Year") + ylab("Average Price") +
    ggtitle(paste0("Average House Prices in ", a)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  b_plot <- ggplot(b_long, aes(x = Year, y = Avg_Price)) +
    geom_point(color = "blue") +
    xlab("Year") + ylab("Average Price") +
    ggtitle(paste0("Average House Prices in ", b)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  c_plot <- ggplot(c_long, aes(x = Year, y = Avg_Price)) +
    geom_point(color = "red") +
    xlab("Year") + ylab("Average Price") +
    ggtitle(paste0("Average House Prices in ", c)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  plots <- grid.arrange(a_plot, b_plot, c_plot, ncol = 2, nrow = 2)

  return(plots)
}

