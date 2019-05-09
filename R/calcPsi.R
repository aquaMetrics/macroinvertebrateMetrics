#' Proportion of Sediment-sensitive Invertebrates (PSI)
#'
#' A sediment-sensitive macro-invertebrate metric that provides a proxy to
#' describe the extent to which the surface of river bed are composed, or
#' covered by sediments. It can be calculated at Taxonomic Levels 3, 4 & 5
#' @param ecologyResults
#' Dataframe with at least three columns
#' item{SAMPLE_ID} - unique idenftier for each sample
#' item{TAXON} - Taxon name that matches to macroinvertebrateTaxa dataset
#' item{RESULT} - Log abundance category
#' @return Dataframe with
#' item{SAMPLE_ID}
#' item{ANALYSIS_NAME}
#' item{DETERMINAND}
#' item{RESULT}
#' @export
#'
#' @examples
#' sample <- demoEcologyResults
#' sample <- filterPsi(sample,taxaList = "TL3")
#' calcPsi(ecologyResults= sample)
calcPsi <- function(ecologyResults, taxaList = "TL3") {
  if (!taxaList %in% c("TL3", "TL5", "TL4")) {
    stop("taxaList arugment must be either 'TL3', 'TL4' or 'TL5'")
  }
  # unique taxa incase any duplicate taxa causes double counting. Metric is based on present / absent so
  # don't need to group by abundance etc.
  ecologyResults <- unique(ecologyResults[, c("SAMPLE_ID", "TAXON", "RESULT")])

  # merge ecology results with taxa metric scores based on taxon name
  macroinvertebrates <-  macroinvertebrateMetrics::macroinvertebrateTaxa
  ecologyResults <-
    merge(ecologyResults,
          macroinvertebrates,
          by.x = "TAXON",
          by.y = "TAXON_NAME")
  psiSensitivityScore <-  macroinvertebrateMetrics::psiSensitivityScore
  ecologyResults <-
    merge(ecologyResults,
          psiSensitivityScore,
          by.x = c("PSI_GROUP", "RESULT"),
          by.y = c("GROUP", "LOG10.ABUN.CAT"))


  # split by sample number
  sampleMetric <-
    lapply(split(ecologyResults, ecologyResults$SAMPLE_ID), function(sample) {
      # calculate PSI score
      sampleMetric <-
        sum(sample$SEDIMENT.SS[sample$PSI_GROUP %in% c("A", "B")], na.rm = TRUE) /
        sum(sample$SEDIMENT.SS[sample$SEDIMENT.SS != "" | is.na(sample$SEDIMENT.SS)], na.rm = T) * 100
      # calculate PSI condition using psiCondition dataframe saved in package
       psiConditions <- macroinvertebrateMetrics::psiCondition
      intervals <-
        data.frame(value = table(cut(
          sampleMetric,
          breaks = c(0, psiConditions[, "upperlimit"]),
          include.lowest = TRUE
        )))
      intervals$row <- row.names(intervals)
      # merge condition lookup table with results
      intervalCondition <-
        merge(psiConditions, intervals, by.x = "value", by.y = "row")
      # create list of psi score and psi condition
      psiResult <- c(sampleMetric,
                     as.character(intervalCondition$condition[intervalCondition$value.Freq == 1]))
      # create dataframe of results
      samplePsi <- data.frame(
        SAMPLE_ID = unique(sample$SAMPLE_ID),
        ANALYSIS_REPNAME = paste0("Proportion of Sediment-sensitive Inverts"),
        ANALYSIS_NAME = paste0("METRIC PSI"),
        DETERMINAND = c(paste0("PSI Score ", taxaList), paste0("PSI Condition ", taxaList)),
        RESULT = psiResult
      )

      return(samplePsi)
    })
  metric <- do.call("rbind", sampleMetric)
  return(metric)
}
