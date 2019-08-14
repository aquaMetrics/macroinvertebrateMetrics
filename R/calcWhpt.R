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
#' @param taxonTable
#' Optional Dataframe with WHPT scores and taxa. Default is NULL and will use built in WHPT scores. But
#' you could supply custom dataframe if required for experimenation/development purposes.
#' @return
#' Dataframe with four columns: SAMPLE_ID, DETERMINAND, RESULT, ANALYSIS_NAME, ANALYSIS_REPNAME
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @examples
#' metricResults <- calcWhpt(demoEcologyResults)
calcWhpt <- function(ecologyResults, taxonTable = NULL) {

  # tidy TAXON name incase of whitespace
  ecologyResults$TAXON <- trimws(ecologyResults$TAXON)

  # get macroinvertebrtae taxa table
  macroinvertebrates <-  macroinvertebrateMetrics::macroinvertebrateTaxa

  if (!is.null(taxonTable)){
    macroinvertebrates <- taxonTable
  }

  # filter results so only Taxon abundance results and greater zero as these
  # are the results required to calculate abundance based WHPT
  ecologyResults <-
    dplyr::mutate(ecologyResults, RESULT = as.numeric(as.character(RESULT))) %>%
    dplyr:: filter(RESULT > 0)

  # TAXON_NAME is factor so convert to character to match ecology results dataframe
  macroinvertebrates$TAXON_NAME <- as.character(macroinvertebrates$TAXON_NAME)
  # Need to join ecology results to reference table of WHPT scores
  ecologyResults <-
    dplyr::inner_join(ecologyResults,
                      macroinvertebrates,
                      by = c("TAXON" = "TAXON_NAME"))

  # group by sample so WHPT scores are produce by sample
  # and group by TL2_TAXON to sum abundance across sub-families/genus
  metricResults <- ecologyResults %>%
    dplyr::group_by(SAMPLE_ID, TL2_TAXON) %>%
    dplyr::summarise(RESULT = sum(RESULT),
                     WHPT_D = mean(WHPT_D),
                     WHPT_C = mean(WHPT_C),
                     WHPT_B = mean(WHPT_B),
                     WHPT_A = mean(WHPT_A))

  # Find correct WHPT score based on abundance categories to use
  # and add to new column 'score'
  metricResults <-
    dplyr::mutate(metricResults, score = dplyr::if_else(RESULT > 999, WHPT_D,
                                            dplyr::if_else(RESULT > 99, WHPT_C,
                                               dplyr::if_else(RESULT > 9, WHPT_B, WHPT_A)
                                           )))

  # calculate WHPT score
  metricResults <-  dplyr::summarise(metricResults,
      WHPT_SCORE = sum(score, na.rm = T),
      WHPT_ASPT = mean(score, na.rm = T),
      WHPT_NTAXA = length(score[!is.na(score)])
    )

  # create final output in standard 'long' format
  whptResult <- metricResults %>%
    tidyr::gather(key = DETERMINAND, value = RESULT, -SAMPLE_ID) %>%
    dplyr::mutate(ANALYSIS_NAME = "METRIC WHPT", ANALYSIS_REPNAME = "WHPT Metric")

  return(whptResult)
}
