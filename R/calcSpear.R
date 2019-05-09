#' SPEcies At Risk (SPEAR) metric
#'
#' Indicator based on biological traits used to detect effects of pesticides on
#' non-target freshwater invertebrate organisms. It can be calculated at Taxonomic
#' Levels 2, 4 and 5.
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
#' @param taxaList
#' The taxonomic level the sample(s) have been identified at according to specificed taxa lists
#' as described in WFD100 Further Development of River Invertebrate Classification Tool.
#' Either "TL2" - Taxa list 2, "TL4" - Taxa list 4 or  "TL5" - Taxa list 5.
#' @return dataframe with metric outputs
#' @references
#' Liess M. & Von der Ohe P. 2005. \emph{Analyzing effects of pesticides on invertebrate
#' communities in streams}. Environ Toxicol Chem. 24: 954-965.
#'
#' Wogram J. & Liess M. 2001. \emph{Rank ordering of macroinvertebrate species sensitivity
#' to toxic compounds by comparison with that of Daphnia magna}. Bull Environ Contam
#' Toxicol. 67: 360-367
#'
#' Liess M., Schaefer R., Schriever C. 2008. \emph{The footprint of pesticide stress in
#' communities - Species traits reveal community effects of toxicants}. Science of
#' the Total Environment. 406: 484-490
#' @seealso
#' \code{\link{filterSpear}}
#' @export
#'
#' @examples
#' ecologyResults <- demoEcologyResults
#' ecologyResults <- ecologyResults[ecologyResults$SAMPLE_NUMBER == 3201863,]
#' ecologyResults <- ecologyResults[ecologyResults$ANALYSIS_NAME == "FW_TAX_ID",]
#' sample <- filterSpear(ecologyResults, taxaList = "TL2")
#' spearOutput <- calcSpear(sample, taxaList = "TL2")

calcSpear <- function(ecologyResults, recoveryArea = "unknown", taxaList = "TL2") {
  sampleMetric <-
    lapply(split(ecologyResults, ecologyResults$SAMPLE_ID), function(sample) {

      # Calculate log10 of Abundance
      # RIVPACS has different criteria when dealing for TL2 and TL5 items
      # which do not score against SPEAR. Maybe it is a "bug"...
      # For TL2 the Log of abundance is calculated no matter the SPEAR score,
      # for TL5, though, the Log of abundance is 0 if SPEAR is null, that is,
      # the taxon does not have score in SPEAR database. I think the TL5 method
      # should be the correct way to go.
      sample$scoringTaxaLog <- log10(sample$RESULT + 1)

      if (taxaList == "TL5") {
        # remove taxa not included in SPEAR
        sample$scoringTaxaLog[is.na(sample$SPEAR_SPECIES)] <- 0
      }

      # calculate sum of Log10 of the abundance (RESULT) for each scoring taxa
      abundanceLogSumScoring <- sum(sample$scoringTaxaLog[sample$SPEAR_SPECIES == "TRUE"], na.rm = T)

      # calculate sum of log abundance of all taxa (scoring or non-scoring)
      abundanceLogSumNonScoring <- sum(sample$scoringTaxaLog)

      # calculate number Of Taxa
      numberOfTaxa <- length(sample$RESULT[!is.na(sample$SPEAR_SPECIES)])

      # calcualte SPEAR ratio
      spearRatio <- (numberOfTaxa * abundanceLogSumScoring) / (numberOfTaxa * abundanceLogSumNonScoring)
      # calculate SPEAR ratio as percentage
      spearRatio <- spearRatio * 100
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
        spearRatio + toxicantExposureCoeff$p2[toxicantExposureCoeff$recoveryArea == recoveryArea]
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
      # calculate SPEAR condition using spearWaterQuality dataframe
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
                       as.character(intervalCondition$class[intervalCondition$value.Freq == 1]))
      # create dataframe of results and add results into it
      samplePsi <- data.frame(
        SAMPLE_ID = unique(sample$SAMPLE_ID),
        ANALYSIS_REPNAME = "Species At Risk from pesticides (SPEAR)",
        ANALYSIS_NAME = "METRIC SPEARpesticides",
        DETERMINAND = c("SPEAR ratio", "SPEAR toxic ratio", "SPEAR class"),
        RESULT = spearResult
      )
    })
  metric <- do.call("rbind", sampleMetric)
  # need to identify SPEAR outputs by the Taxa list to separate TL5 and TL2 outputs
  metric$DETERMINAND <- paste(metric$DETERMINAND, taxaList)
  return(metric)
}
