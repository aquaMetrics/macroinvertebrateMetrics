#' Acid WFD-AWIC metric
#'
#' @param data dataframe containing mixtaxon invertebrates
#'
#' @return dataframe
#' @export
#' @importFrom dplyr n select
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr "%>%"
#' @examples
#' metricResults <- calc_awic(demoEcologyResults)
calc_awic <- function(data) {
  data <- data[data$ANALYSIS_NAME == "MIXTAX_TST" &
    data$DETERMINAND == "Taxon abundance", ]

  data <- data[, c("SAMPLE_NUMBER", "TAXON", "VALUE")]

  # Note, most common error: "Error in data$ANALYSIS_NAME : object of type
  # 'closure' is not subsettable" Means your dataframe name matches a function
  # name. e.g. data() function and probably means you forgot to run the code to
  # create the data.frame in the first place.

  # Filter macroinvertebrate scores for only AWIC scores
  # This helps keep the data comprehensible and only things we are interested in
  taxa <- macroinvertebrateTaxa
  taxa <- taxa[, c("TAXON_NAME", "AWIC_A", "AWIC_B", "AWIC_C")]

  # Merge results and scores
  results <- merge(data, taxa, by.x = "TAXON", by.y = "TAXON_NAME")

  # Create awic_score column --------------------------------------------------
  results$awic_score <- NA

  # Replace value in awic_score column with matching values in AWIC_A, AWIC_B,
  # AWIC_C based on 'VALUE' in this case value is Abundance.
  results$awic_score[results$VALUE > 0] <- results$AWIC_A[results$VALUE > 0]
  results$awic_score[results$VALUE > 9] <- results$AWIC_B[results$VALUE > 9]
  results$awic_score[results$VALUE > 99] <- results$AWIC_C[results$VALUE > 99]

  results <- na.omit(results)

  # Group by SAMPLE_NUMBER to calculate score per sample
  results <- dplyr::group_by(results, SAMPLE_NUMBER)

  # `summarise` calculates `sum()` and `n()` on what we grouped by(sample_number)
  scores <- dplyr::summarise(results,
    sample_score = sum(awic_score),
    ntaxa = n()
  )


  # Create (or 'mutate') a new column called wfd_awic (sample_score / ntaxa)
  scores <- scores %>% dplyr::mutate(wfd_awic = sample_score / ntaxa)

  scores <- pivot_longer(scores,
    cols = c("sample_score", "ntaxa", "wfd_awic"),
    names_to = "DETERMINAND",
    values_to = "RESULT"
  )

  scores <- select(scores,
    "SAMPLE_ID" = SAMPLE_NUMBER,
    "DETERMINAND" = DETERMINAND,
    "RESULT" = RESULT)

  scores <- mutate(scores,
                   "ANALYSIS_NAME" = "WFD_AWIC",
                   "ANALYSIS_REPNAME" = "WFD AWIC")

  return(scores)
}
