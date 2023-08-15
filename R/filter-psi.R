#' Filter results for PSI metric
#'
#' @param ecologyResults
#' Dataframe of ecology results with mandatory four columns:
#' \itemize{
#'  \item RESULT - Numeric count result.
#'  \item TAXON - Taxon name matching names in macroinvertebrates::macroinvertebrateTaxa table
#'  \item DETERMINAND - 'Taxon abundance'
#'  \item SAMPLE_ID - Unique identifier for each sample (number or text)
#' }
#' @param taxa_list
#' The taxonomic level the sample(s) have been identified at according to
#' specified taxa lists as described in WFD100 Further Development of River
#' Invertebrate Classification Tool. Either "TL2" - Taxa list 2, "TL3" - Taxa
#' list 2 or "TL5" Taxa list 5 or "TL4". PSI TL lists don't match RIVPACS
#' species e.g. TL3 list includes 'Oligochaeta' even though it is not a TL3 taxa
#' according to WFD100.
#' @return
#' Dataframe
#' @export
#'
#' @examples
#' filter_psi(demo_data, taxa_list = "TL2")
filter_psi <- function(ecologyResults, taxa_list = "TL3") {
  if (!taxa_list %in% c("TL2", "TL3", "TL5", "TL4")) {
    stop("taxa_list arugment must be either 'TL2', 'TL3', 'TL4' or 'TL5'")
  }
  # only need Taxon abundance determinand
  ecologyResults <-
    ecologyResults[ecologyResults$question == "Taxon abundance" |
      ecologyResults$question == "Taxon Abundance", ]
  # merge ecology results with taxa metric scores based on taxon name
  macroinvertebrateTaxa <- macroinvertebrateMetrics::macroinvertebrateTaxa
  ecologyResults$label <- trimws(ecologyResults$label)
  taxaMetricValues <-
    merge(ecologyResults,
      macroinvertebrateTaxa,
      by.x = "label",
      by.y = "TAXON_NAME"
    )
  # if nothing in data.frame
  if (length(taxaMetricValues$label) == 0) {
    return(taxaMetricValues)
  }
  # change class for aggregation
  taxaMetricValues$PSI_GROUP <- as.character(taxaMetricValues$PSI_GROUP)
  # aggregate to correct Taxa List (TL) level
  taxaMetricValues$response <- as.character(taxaMetricValues$response)
  taxaMetricValues$response <- as.numeric(taxaMetricValues$response)
  if (taxa_list == "TL3") {
    taxaMetricValues$TL3_TAXON <- as.character(taxaMetricValues$TL3_TAXON)
    taxaMetricValues$TL3_TAXON[taxaMetricValues$label == "Oligochaeta"] <- "Oligochaeta"

    taxaMetricValues <- stats::aggregate(
      taxaMetricValues[, c("RESULT")],
      by = list(
        taxaMetricValues$sample_id,
        taxaMetricValues$TL3_TAXON,
        taxaMetricValues$PSI_GROUP
      ),
      FUN = sum
    )
  }

  if (taxa_list == "TL2") {
    taxaMetricValues <- stats::aggregate(
      taxaMetricValues[, c("response")],
      by = list(
        taxaMetricValues$sample_id,
        taxaMetricValues$TL2_TAXON,
        taxaMetricValues$PSI_GROUP
      ),
      FUN = sum
    )
  }

  if (taxa_list == "TL4") {
    taxaMetricValues <- stats::aggregate(
      taxaMetricValues[, c("response")],
      by = list(
        taxaMetricValues$sample_id,
        taxaMetricValues$TL4_TAXON,
        taxaMetricValues$PSI_GROUP
      ),
      FUN = sum
    )
  }

  if (taxa_list == "TL5") {
    taxaMetricValues <- stats::aggregate(
      taxaMetricValues[, c("response")],
      by = list(
        taxaMetricValues$sample_id,
        taxaMetricValues$TL5_TAXON,
        taxaMetricValues$PSI_GROUP
      ),
      FUN = sum
    )
  }

  if (taxa_list == "TL4") {
    taxaMetricValues <- stats::aggregate(
      taxaMetricValues[, c("response")],
      by = list(
        taxaMetricValues$sample_id,
        taxaMetricValues$TL5_TAXON,
        taxaMetricValues$PSI_GROUP
      ),
      FUN = sum
    )
  }
  # update names after aggregation
  names(taxaMetricValues) <- c("sample_id", "label", "PSI_GROUP", "response")
  # PSI_GROUP not required by calcPSI function
  taxaMetricValues$PSI_GROUP <- NULL
  # remove 'blank' taxa if not scores for PSI
  taxaMetricValues <- taxaMetricValues[taxaMetricValues$label != "", ]
  taxaMetricValues <- taxaMetricValues[!is.na(taxaMetricValues$label), ]
  # log Abundance
  taxaMetricValues$response <- floor(log10(taxaMetricValues$response) + 1)
  taxaMetricValues$response[taxaMetricValues$response > 6] <- 6
  return(taxaMetricValues)
}
