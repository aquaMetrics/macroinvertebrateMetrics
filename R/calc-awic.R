#' Acid WFD-AWIC metric
#'
#' @param data dataframe containing mixtaxon invertebrates
#'
#' @return dataframe
#' @export
#' @importFrom dplyr n select mutate
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @examples
#' metricResults <- calc_awic(demoEcologyResults)
calc_awic <- function(data) {
  data <- data[, c("SAMPLE_NUMBER", "TAXON", "VALUE")]

  # Filter macroinvertebrate scores for only AWIC scores
  # This helps keep the data comprehensible and only things we are interested in
  taxa <- macroinvertebrateMetrics::macroinvertebrateTaxa
  taxa <- taxa[, c("TAXON_NAME", "AWIC_A", "AWIC_B", "AWIC_C")]

  # Merge results and scores
  results <- merge(data, taxa, by.x = "TAXON", by.y = "TAXON_NAME")
  if (nrow(results) < 1) {
    return(NULL)
  }
  # Create awic_score column --------------------------------------------------
  results$awic_score <- NA

  # Filter out NA values (if data contains non-numeric values)
  results <- results[!is.na(results$VALUE), ]
  # Replace value in awic_score column with matching values in AWIC_A, AWIC_B,
  # AWIC_C based on 'VALUE' in this case value is Abundance.
  results$awic_score[results$VALUE > 0] <- results$AWIC_A[results$VALUE > 0]
  results$awic_score[results$VALUE > 9] <- results$AWIC_B[results$VALUE > 9]
  results$awic_score[results$VALUE > 99] <- results$AWIC_C[results$VALUE > 99]

  results <- stats::na.omit(results)

  # Group by SAMPLE_NUMBER to calculate score per sample
  results <- dplyr::group_by(results, .data$SAMPLE_NUMBER)

  # `summarise` calculates `sum()` and `n()` on what we grouped by(sample_number)
  scores <- dplyr::summarise(results,
    sample_score = sum(.data$awic_score),
    ntaxa = n()
  )


  # Create (or 'mutate') a new column called wfd_awic (sample_score / ntaxa)
  scores <- scores %>%
    mutate(wfd_awic = .data$sample_score / .data$ntaxa)

  scores <- pivot_longer(scores,
    cols = c("sample_score", "ntaxa", "wfd_awic"),
    names_to = "DETERMINAND",
    values_to = "RESULT"
  )

  scores <- select(scores,
    "SAMPLE_ID" = SAMPLE_NUMBER,
    "DETERMINAND" = DETERMINAND,
    "RESULT" = RESULT
  )

  scores <- mutate(scores,
    "ANALYSIS_NAME" = "WFD_AWIC",
    "ANALYSIS_REPNAME" = "WFD AWIC"
  )
  scores$SAMPLE_ID <- as.character(scores$SAMPLE_ID)
  return(scores)
}
