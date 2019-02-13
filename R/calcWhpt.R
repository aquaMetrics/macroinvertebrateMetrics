#' Calculate WHPT scores
#'
#' @param ecologyResults
#' dataframe of taxonomic results with mandatory four columns
#' SAMPLE_ID, DETERMINAND, RESULT, TAXON
#' @param taxonTable
#' dataframe with WHPT scores and taxa
#' @return
#' Dataframe with four columns: SAMPLE_ID, DETERMINAND, RESULT, ANALYSIS_NAME, ANALYSIS_REPNAME
#' @export
#'
#' @examples
#' metricResults <- calcWhpt(demoEcologyResults)
calcWhpt <- function(ecologyResults, taxonTable = NULL) {
  # get macroinvertebrtae taxa
  macroinvertebrates <-  macroinvertebrateMetrics::macroinvertebrateTaxa

  if (!is.null(taxonTable)){
    macroinvertebrates <- taxonTable
  }

  # filter results so only Taxon abundance results and greater zero as these
  # are the results required for to calculate abundance based WHPT
  ecologyResults <-
    dplyr::filter(ecologyResults,
                  DETERMINAND %in% c("Taxon abundance", "Taxon Abundance")) %>%
    mutate(RESULT = as.numeric(as.character(RESULT))) %>% filter(RESULT > 0)

  # Need to join ecology results to reference table of WHPT scores
  ecologyResults <-
    dplyr::inner_join(ecologyResults,
                      macroinvertebrates,
                      by = c("TAXON" = "TAXON_NAME"))

  # WHPT score is based on abundance - find correct WHPT score to use
  metricResults <-
    dplyr::mutate(ecologyResults, score = if_else(RESULT > 999, WHPT_D,
                                           if_else(
                                             RESULT > 99, WHPT_C,
                                             if_else(RESULT > 9, WHPT_B, WHPT_A)
                                           )))

  # group by sample so WHPT scores are produce by sample
  metricResults <- metricResults %>%
    dplyr::group_by(SAMPLE_ID, TL2_TAXON) %>%
    dplyr::summarise(score = mean(score))

  # calculate WHPT score
  metricResults <- metricResults %>%
    dplyr::summarise(
      WHPT_SCORE = sum(score, na.rm = T),
      WHPT_ASPT = mean(score, na.rm = T),
      WHPT_NTAXA = length(score[!is.na(score)])
    )

  # create final output in standard 'long' format
  whptResult <- metricResults %>%
    tidyr::gather(key = DETERMINAND, value = RESULT, -SAMPLE_ID) %>%
    dplyr::mutate(ANALYSIS_NAME = "WHPT Metric", ANALYSIS_REPNAME = "WHPT Metric")

  return(whptResult)
}
