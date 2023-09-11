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
  # group by sample so WHPT scores are produce by sample
  # and group by TL2_TAXON to sum abundance across sub-families/genus
  metric_results <- data %>%
    dplyr::group_by(.data$sample_id, .data$TL2_TAXON) %>%
    dplyr::summarise(
      response = sum(.data$response),
      WHPT_D = mean(.data$WHPT_D),
      WHPT_C = mean(.data$WHPT_C),
      WHPT_B = mean(.data$WHPT_B),
      WHPT_A = mean(.data$WHPT_A),
      WHPT_P = mean(.data$WHPT_P)
    )

  # Find correct WHPT score based on abundance categories to use
  # and add to new column 'score'
  metric_results <-
    dplyr::mutate(metric_results,
      score = dplyr::if_else(.data$response > 999, .data$WHPT_D,
        dplyr::if_else(.data$response > 99, .data$WHPT_C,
          dplyr::if_else(.data$response > 9,
            .data$WHPT_B,
            .data$WHPT_A
          )
        )
      )
    )

  # calculate WHPT score
  metric_results <- dplyr::summarise(metric_results,
    WHPT_SCORE = sum(.data$score, na.rm = TRUE),
    WHPT_ASPT = mean(.data$score, na.rm = TRUE),
    WHPT_NTAXA = length(.data$score[!is.na(.data$score)]),
    WHPT_P_SCORE = sum(.data$WHPT_P, na.rm = TRUE),
    WHPT_P_ASPT = mean(.data$WHPT_P, na.rm = TRUE),
    WHPT_P_NTAXA = length(.data$WHPT_P[!is.na(.data$WHPT_P)])
  )
  # create final output in standard 'long' format
  whpt_result <- metric_results %>%
    tidyr::gather(
      key = !!column_attributes$name[2],
      value = !!column_attributes$name[3], -sample_id
    ) %>%
    dplyr::mutate(
      !!column_attributes$name[5] := "WHPT Metric",
      !!column_attributes$name[6] := "WHPT Metric"
    )

  whpt_result <- dplyr::mutate_at(
    whpt_result,
    column_attributes$name[3], as.character
  )

  return(whpt_result)
}
