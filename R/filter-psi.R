#' Filter results for PSI metric
#'
#' @param data
#' Dataframe of ecology results with mandatory four columns:
#' \itemize{
#'  \item response - Numeric count result.
#'  \item label - Taxon name matching names in macroinvertebrates::macroinvertebrateTaxa table
#'  \item question - 'Taxon abundance'
#'  \item sample_id - Unique identifier for each sample (number or text)
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
filter_psi <- function(data, taxa_list = taxa_list) {
  if (!taxa_list %in% c("TL2", "TL3", "TL5", "TL4")) {
    stop("taxa_list arugment must be either 'TL2', 'TL3', 'TL4' or 'TL5'")
  }

  if (length(data$label) == 0) {
    return(data)
  }
  # change class for aggregation
  data$PSI_GROUP <- as.character(data$PSI_GROUP)
  # aggregate to correct Taxa List (TL) level
  data$response <- as.character(data$response)
  data$response <- as.numeric(data$response)
  if (taxa_list == "TL3") {
    data$TL3_TAXON <- as.character(data$TL3_TAXON)
    data$TL3_TAXON[data$label == "Oligochaeta"] <- "Oligochaeta"

    data <- stats::aggregate(
      data[, c("response")],
      by = list(
        data$sample_id,
        data$TL3_TAXON,
        data$PSI_GROUP
      ),
      FUN = sum
    )
  }

  if (taxa_list == "TL2") {
    data <- stats::aggregate(
      data[, c("response")],
      by = list(
        data$sample_id,
        data$TL2_TAXON,
        data$PSI_GROUP
      ),
      FUN = sum
    )
  }

  if (taxa_list == "TL4") {
    data <- stats::aggregate(
      data[, c("response")],
      by = list(
        data$sample_id,
        data$TL4_TAXON,
        data$PSI_GROUP
      ),
      FUN = sum
    )
  }

  if (taxa_list == "TL5") {
    data <- stats::aggregate(
      data[, c("response")],
      by = list(
        data$sample_id,
        data$TL5_TAXON,
        data$PSI_GROUP
      ),
      FUN = sum
    )
  }

  if (taxa_list == "TL4") {
    data <- stats::aggregate(
      data[, c("response")],
      by = list(
        data$sample_id,
        data$TL5_TAXON,
        data$PSI_GROUP
      ),
      FUN = sum
    )
  }
  # update names after aggregation
  names(data) <- c("sample_id", "label", "PSI_GROUP", "response")
  # PSI_GROUP not required by calcPSI function
  data$PSI_GROUP <- NULL
  # remove 'blank' taxa if not scores for PSI
  data <- data[data$label != "", ]
  data <- data[!is.na(data$label), ]
  # log Abundance
  if (nrow(data) == 0) {
    return(NULL)
  }
  data$response <- floor(log10(data$response) + 1)
  data$response[data$response > 6] <- 6
  return(data)
}
