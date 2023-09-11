#' Acid WFD-AWIC metric
#'
#' @param data dataframe containing mixtaxon invertebrates
#' @param names
#' Optional, user provided list of column names different to those used
#' in `column_attributes` to match with input data
#' @param questions
#' Optional, user provided 'question' default is 'Taxon abundance', which
#' filters only abundance values.
#' @return dataframe
#' @export
#' @importFrom dplyr n select mutate any_of
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data :=
#' @examples
#' metricResults <- calc_awic(demo_data)
calc_awic <- function(data,
                      names = macroinvertebrateMetrics::column_attributes$name,
                      questions = c(
                        "Taxon abundance",
                        "Taxon Abundance"
                      ),
                      metric_cols = macroinvertebrateMetrics::metric_cols) {
  data <- calc_metric(data,
    metrics = "awic",
    names = names,
    questions = questions,
    metric_cols = metric_cols
  )

  return(data)
}
