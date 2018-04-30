#' Filter results for PSI metric
#'
#' @param ecologyResults
#' Dataframe with ecology results...
#' @param taxaList
#' The taxonomic level the sample(s) have been identified at according to specificed taxa lists
#' as described in \href{https://www.sniffer.org.uk/Handlers/Download.ashx?IDMF=e9b55f14-59cf-46b4-927f-66411e2e02d8}
#' {WFD100 Further Development of River Invertebrate Classification Tool}.
#' Either "TL2" - Taxa list 2 or "TL5" Taxa list 3.
#' @return
#' Dataframe
#' @export
#'
#' @examples
#' filterPsi(demoEcoloyResults, taxaList = "TL2")
filterPsi <- function(ecologyResults, taxaList = "TL2") {
  # only need Taxon abundance determinand
  ecologyResults <- ecologyResults[ecologyResults$DETERMINAND == "Taxon abundance" |
                                     ecologyResults$DETERMINAND == "Taxon Abundance", ]
  # merge ecology results with taxa metric scores based on taxon name
  taxaMetricValues <- merge(ecologyResults, macroinvertebrateTaxa, by.x = "TAXON", by.y = "TAXON_NAME")
  # if nothing in data.frame
  if (length(taxaMetricValues$TAXON) == 0) {
    return(taxaMetricValues)
  }
  # aggregate to correct Taxa List (TL) level
  taxaMetricValues$RESULT <-  as.character(taxaMetricValues$RESULT)
  taxaMetricValues$RESULT <-  as.numeric(taxaMetricValues$RESULT)
  if (taxaList == "TL2") {
    taxaMetricValues <- aggregate(taxaMetricValues[, c("RESULT")],
                                  by = list(taxaMetricValues$SAMPLE_NUMBER,
                                            taxaMetricValues$TL2_TAXON,
                                            taxaMetricValues$PSI_GROUP), FUN = sum)
  } else {
    taxaMetricValues <- aggregate(taxaMetricValues[, c("RESULT")],
                                  by = list(taxaMetricValues$SAMPLE_NUMBER,
                                            taxaMetricValues$TL2_TAXON,
                                            taxaMetricValues$PSI_GROUP), FUN = sum)
  }
  # update names after aggregation
  names(taxaMetricValues) <- c("SAMPLE_NUMBER", "TAXON", "PSI_GROUP", "RESULT")
  return(taxaMetricValues)
}