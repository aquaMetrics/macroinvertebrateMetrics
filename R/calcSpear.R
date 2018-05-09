#' Calculate SPEAR metric
#'
#' @param ecologyresults
#' Dataframe of results with four columns: SAMPLE_NUMBER, TAXON, SPEAR_SPECIES, RESULT
#' @param recoveryArea
#' There are 3 different paramaters depending on availability of "Recovery areas" information:
#' \itemize{
#'   \item "Absent" - No presence of recovery areas
#'   \item "Present" - Presence of recovery areas
#'   \item "Unknown" - No information available
#' }
#' @return dataframe with...
#' @export
#'
#' @examples
#' calcSpear(demoEcoloyResults, recoveryArea = "unknown")
#'
#' instal.git_()
calcSpear <- function(ecologyResults, recoveryArea = "unknown") {
  sampleMetric <-
    lapply(split(ecologyResults, ecologyResults$SAMPLE_ID), function(sample) {
      # calculate SPEAR score
      scoringTaxa <-
        length(sample$SPEAR_SPECIES[sample$SPEAR_SPECIES == TRUE])
      scoringTaxaLog <-
        log(length(sample$SPEAR_SPECIES[sample$SPEAR_SPECIES == TRUE]))
      spearRatio  <- ifelse(scoringTaxa > 0,
                            100 / scoringTaxa * scoringTaxaLog, 0)
      # regression coefficients for Toxicant Exposure calculation
      toxicantExposureCoeff <-
        data.frame(
          recoveryArea = c("absent", "presence", "unknown"),
          p1 = c(1 / -8.02, 1 / -6.16, 1 / -7),
          p2 = c(-1.28 / -8.02, -20.07 / -6.16, -10.675 / -7)
        )
      # toxicant exposure, depending on availability of Recovery area information
      spearToxicantRatio <-
        toxicantExposureCoeff$p1[toxicantExposureCoeff$recoveryArea == recoveryArea] *
        spearRatio +   toxicantExposureCoeff$p2[toxicantExposureCoeff$recoveryArea == recoveryArea]
      # SPEAR water quality classes
      # Bad:      <= 11% SPEAR
      # Poor:     > 11% and <= 22% SPEAR
      # Moderate: > 22% and <= 33% SPEAR
      # Good:     > 33% and <= 44% SPEAR
      # High:     > 44% SPEAR
      spearWaterQuality <-
        data.frame(
          class = c("Bad", "Poor", "Moderate", "Good", "High"),
          upperlimit = c(11, 22, 33, 44, 100),
          value = seq(1:5)
        )
      # calculate SPEAR condition using psiCondition dataframe saved in package
      intervals <-
        data.frame(value = table(cut(
          spearRatio,
          breaks = c(0, spearWaterQuality[, "upperlimit"]),
          include.lowest = TRUE
        )))
      intervals$row <- row.names(intervals)
      # merge condition lookup table with results
      intervalCondition <-
        merge(spearWaterQuality,
              intervals,
              by.x = "value",
              by.y = "row")
      # create list of the three results: SPEAR score, SPEAR condition, SPEAR class
      spearResult <- c(spearRatio,
                       spearToxicantRatio,
                       as.character(intervalCondition$class[intervalCondition$value == 1]))
      # create dataframe of results and add results into it
      samplePsi <- data.frame(
        SAMPLE_ID = unique(sample$SAMPLE_ID),
        ANALYSIS_REPNAME = "SPEAR Metric ",
        ANALYSIS_NAME = "SPEAR METRIC ",
        DETERMINAND = c("SPEAR ratio", "SPEAR toxic ratio", "SPEAR class"),
        RESULT = spearResult
      )
    })
  metric <- do.call("rbind", sampleMetric)
  return(metric)
}