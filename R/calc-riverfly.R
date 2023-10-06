#' Riverfly metric
#'
#' Function to calculate riverfly scores
#'
#' @param data
#' A data frame of ecology data as defined in `column_attributes`.
#' @param names
#' Optional, user provided list of column names different to those used
#' in `column_attributes` to match with input data
#' @param questions
#' Optional, user provided 'question' default is 'Taxon abundance', which
#' filters only abundance values.
#' @param metric_cols Columns used from taxon table to calculate metrics and
#'   the taxon name column to join to input data (if using custom taxon table)
#' @param ... Pass in specific paramters for each metric.
#' @return
#' A data frame 5 variables
#' @export
#' @importFrom rlang .data :=
#' @importFrom magrittr "%>%"
#' @importFrom dplyr all_of across
#' @examples
#' data <- demo_data
#' data <- data[data$parameter == "River Family Inverts", ]
#' output <- calc_riverfly(data)
#'
calc_riverfly <- function(data,
                          names = macroinvertebrateMetrics::column_attributes$name,
                          questions = c(
                            "Taxon abundance",
                            "Taxon Abundance",
                            "Live abundance"),
                            metric_cols = macroinvertebrateMetrics::metric_cols
                            ){
  # To allow user to specify the names of the columns to match the columns in
  # their dataset update package column name data with column names provided to
  # function
  column_attributes <- macroinvertebrateMetrics::column_attributes
  column_attributes$name <- names
  # Validate and format input data - column names are user provided or revert to
  # default. After this point, columns names are referred by index/number rather
  # than text of column name, this allows the default column names to be update
  # easily in future
  data <- validate_input(data,
                         names = names,
                         questions = questions,
                         metric_cols = metric_cols
                         )

  data <- calc_metric(data,
                      metrics = "riverfly",
                      names = names,
                      questions = questions,
                      metric_cols = metric_cols
  )

  return(data)
}
