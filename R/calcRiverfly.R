#' Riverfly metric
#'
#' Function to calculate riverfly scores
#'
#' @param ecologyResults
#' A data frame of ecology view data
#' @return
#' A data frame 5 variables
#' @export
#'
#' @examples
#' ecologyResults <- demoEcologyResults
#' ecologyResults <- ecologyResults[ecologyResults$ANALYSIS_NAME == "FW_TAX_ID",]
#' spearOutput <- calcRiverfly(ecologyResults)

calcRiverfly <- function(ecologyResults = ecologyResults) {

# need to create riverfly score for each sample number
SEPAresults <-
    merge(ecologyResults,
          macroinvertebrateTaxa,
          by.x = "TAXON",
          by.y = "TAXON_NAME")
# this table has a lookup list for riverfly taxon groups against TL2 families
taxonTable <- read.csv(file = "inst/extdata/RIVERFLY.csv")

# merge only works for TL2 currently - will need to merge with invert taxa table to do Tl3, Tl5 etc
riverflyTaxa <-
  merge(
    SEPAresults[,c("TL2_5_TAXON","TAXON","VALUE","SAMPLE_ID")],
    taxonTable,
    by.x = "TL2_5_TAXON",
    by.y = "REPORTED_NAME"
  )

riverflySum <- dplyr::group_by(riverflyTaxa, RIVERFLY_GROUP, SAMPLE_ID) %>%  dplyr::summarise(VALUE = sum(VALUE))

# riverfly abundance categories
category <- function(x) {
  ifelse(x > 99, 3,
    ifelse(x > 9, 2,
     ifelse(x > 0, 1, 0)))
  }
riverflySum$VALUE_LOG <- c(apply(riverflySum[,"VALUE"], 2, category))

# group_by sample_id and sum log abundance
riverflyScore <- dplyr::group_by(riverflySum, SAMPLE_ID) %>% dplyr::summarise(RESULT = sum(VALUE_LOG))
riverflyScore$DETERMINAND <- "Riverfly Score"
riverflyScore$ANALYSIS <- "Metric: Riverfly Score"
riverflyScore$ANALYSIS_REPNAME <- "Metric: Riverfly Score"

return(riverflyScore)
}

