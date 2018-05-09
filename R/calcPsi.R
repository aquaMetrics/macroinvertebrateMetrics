#' Proportion of Sediment-sensitive Invertebrates (PSI)
#'
#' A sediment-sensitive macro-invertebrate metric that provides a proxy to
#' describe the extent to which the surface of river bed are composed, or
#' covered by sediments. It can be calculated at Taxonomic Levels 3, 4 & 5
#' @param ecologyResults
#' Dataframe with at least two columns
#' item{SAMPLE_ID} - unique idenftier for each sample
#' item{TAXON} - Taxon name that matches to macroinvertebrateTaxa dataset
#' @return Dataframe with
#' item{SAMPLE_ID}
#' item{ANALYSIS_NAME}
#' item{DETERMINAND}
#' item{RESULT}
#' @export
#'
#' @examples
#' sample <- demoEcologyResults
#' sample <- filterPsi(sample,taxaList = "TL2")
#' calcPsi(ecologyResults= sample)
calcPsi <- function(ecologyResults, valueWarning = FALSE, taxaList = "TL2") {
  if (!taxaList %in% c("TL2", "TL5", "TL4")) {
    stop("taxaList arugment must be either 'TL2', 'TL4' or 'TL5'")
  }
  # unique taxa incase any duplicate taxa causes double counting. Metric is based on present / absent so
  # don't need to group by abundance etc.
  ecologyResults <- unique(ecologyResults[, c("SAMPLE_ID", "TAXON")])

  # merge ecology results with taxa metric scores based on taxon name
  ecologyResults <-
    merge(ecologyResults,
          macroinvertebrateTaxa,
          by.x = "TAXON",
          by.y = "TAXON_NAME")
  # split by sample number
  sampleMetric <-
    lapply(split(ecologyResults, ecologyResults$SAMPLE_ID), function(sample) {
      # calculate PSI score
      sampleMetric <-
        length(sample$PSI_GROUP[sample$PSI_GROUP %in% c("A", "B")]) /
        length(sample$PSI_GROUP[!is.na(sample$PSI_GROUP)]) * 100
      # calculate PSI condition using psiCondition dataframe saved in package
      intervals <-
        data.frame(value = table(cut(
          sampleMetric,
          breaks = c(0, psiCondition[, "upperlimit"]),
          include.lowest = TRUE
        )))
      intervals$row <- row.names(intervals)
      # merge condition lookup table with results
      intervalCondition <-
        merge(psiCondition, intervals, by.x = "value", by.y = "row")
      # create list of psi score and psi condition
      psiResult <- c(sampleMetric,
                     as.character(intervalCondition$condition[intervalCondition$value.Freq == 1]))
      # create dataframe of results
      samplePsi <- data.frame(
        SAMPLE_ID = unique(sample$SAMPLE_ID),
        ANALYSIS_REPNAME = paste0("PSI Metric ", taxaList),
        ANALYSIS_NAME = paste0("PSI Metric ", taxaList),
        DETERMINAND = c(paste0("PSI Score ", taxaList), paste0("PSI Condition ", taxaList)),
        RESULT = psiResult
      )
      # r warnings
      # r  if() {
      # r   warnings <- data.frame(SAMPLE_ID = unique(sample$SAMPLE_ID),
      # r                          DETERMINAND = "Warning",
      # r                          ANALYSIS_REPNAME = "PSI METRIC",
      # r                          ANALYSIS_NAME = "PSI Metric",
      # r                          RESULT = ### add error here ####
      # r  samplePsi <- rbind(samplePsi, warnings)
      # r  }
      return(samplePsi)
    })
  metric <- do.call("rbind", sampleMetric)
  return(metric)
}