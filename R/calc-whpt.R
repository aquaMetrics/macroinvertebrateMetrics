#' Calculate WHPT scores
#'
#' @param ecologyResults
#' Dataframe of taxonomic results with mandatory three columns:
#' \describe{
#'   \item{SAMPLE_ID}{Unique sample identifier}
#'   \item{RESULT}{Numeric abundance}
#'   \item{TAXON}{Character - TL2 WHPT taxon name}
#'   }
#' Columns names must match these exactly, but the column order does not matter.
#' See demoEcologyResults for example dataset.
#' @param taxon_table
#' Optional Dataframe with WHPT scores and taxa. Default is NULL and will use
#' built in WHPT scores. But you could supply custom dataframe if required for
#' experimenation/development purposes.
#' @param names
#' Optional, user provided list of column names different to those used
#' in `column_attributes` to match with input data
#' @param questions
#' Optional, user provided 'question' default is 'Taxon abundance', which
#' filters only abundance values.
#'
#' @return
#' Dataframe with four columns: SAMPLE_ID, DETERMINAND, RESULT, ANALYSIS_NAME,
#' ANALYSIS_REPNAME
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data :=
#' @importFrom dplyr any_of across
#' @examples
#' metricResults <- calc_whpt(demo_data)
calc_whpt <- function(data,
                      taxon_table = macroinvertebrateMetrics::macroinvertebrateTaxa,
                      names = macroinvertebrateMetrics::column_attributes$name,
                      questions = c(
                        "Taxon abundance",
                        "Taxon Abundance",
                        "Live abundance"
                      ),
                      metric_cols = macroinvertebrateMetrics::metric_cols) {
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
    taxon_table = taxon_table,
    questions = questions,
    metric_cols = metric_cols
  )

  data <- calc_metric(data,
                      metrics = "whpt",
                      names = names,
                      questions = questions,
                      metric_cols = metric_cols
  )

  return(data)
}
