#' Filter Incidents by Type
#'
#' Filters a dataframe of cybersecurity incidents by the specified type.
#'
#' @param df A dataframe of incidents.
#' @param type A string indicating the type of incident (e.g., "malware").
#' @return A filtered dataframe.
#' @export
filter_by_type <- function(df, type) {
  subset(df, incident_type == type)
}

#' Summarize Incident Data
#'
#' Provides summary statistics of the cybersecurity incident data.
#'
#' @param df A dataframe of incidents.
#' @return A summary dataframe.
#' @export
summarize_incidents <- function(df) {
  summary(df)
}

#' Plot Cyber Attack Trends
#'
#' Generates a line plot showing the number of incidents over time.
#'
#' @param df A dataframe containing a `date` column.
#' @return A ggplot object.
#' @export
plot_attack_trends <- function(df) {
  library(ggplot2)
  df$date <- as.Date(df$date)
  df$count <- 1
  df_summary <- aggregate(count ~ date, data = df, FUN = sum)
  ggplot(df_summary, aes(x = date, y = count)) +
    geom_line() +
    labs(title = "Cyber Attack Trends Over Time", x = "Date", y = "Number of Attacks")
}
