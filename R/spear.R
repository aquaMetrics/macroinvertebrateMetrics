#' SPEcies At Risk (SPEAR) metric
#'
#' Indicator based on biological traits used to detect effects of pesticides on
#' non-target freshwater invertebrate organisms. It can be calculated at
#' Taxonomic Levels 2, 4 and 5.
#'
#' @param data
#' Dataframe of results with four columns: SAMPLE_NUMBER, TAXON, SPEAR_SPECIES,
#' RESULT
#' @param recoveryArea
#' There are 3 different paramaters depending on availability of "Recovery areas" information:
#' \itemize{
#'   \item "Absent" - No presence of recovery areas
#'   \item "Present" - Presence of recovery areas
#'   \item "Unknown" - No information available
#' }
#' @param taxa_list The taxonomic level the sample(s) have been identified at
#'   according to specificed taxa lists as described in WFD100 Further
#'   Development of River Invertebrate Classification Tool. Either "TL2" - Taxa
#'   list 2, "TL4" - Taxa list 4 or  "TL5" - Taxa list 5.
#' @return dataframe with metric outputs
#' @references
#' Liess M. & Von der Ohe P. 2005. \emph{Analyzing effects of pesticides on
#' invertebrate communities in streams}. Environ Toxicol Chem. 24: 954-965.
#'
#' Wogram J. & Liess M. 2001. \emph{Rank ordering of macroinvertebrate species
#' sensitivity to toxic compounds by comparison with that of Daphnia magna}.
#' Bull Environ Contam Toxicol. 67: 360-367
#'
#' Liess M., Schaefer R., Schriever C. 2008. \emph{The footprint of pesticide
#' stress in communities - Species traits reveal community effects of
#' toxicants}. Science of the Total Environment. 406: 484-490
#' @seealso
#' \code{\link{filter_spear}}
spear <- function(data, recoveryArea = "unknown", taxa_list = "TL2") {
  column_attributes <- macroinvertebrateMetrics::column_attributes
  # Calculate log10 of Abundance
  # RIVPACS has different criteria when dealing for TL2 and TL5 items
  # which do not score against SPEAR. Maybe it is a "bug"...
  # For TL2 the Log of abundance is calculated no matter the SPEAR score,
  # for TL5, though, the Log of abundance is 0 if SPEAR is null, that is,
  # the taxon does not have score in SPEAR database. I think the TL5 method
  # should be the correct way to go.
  data$scoringTaxaLog <- log10(data[, column_attributes$name[3]] + 1)
  if (taxa_list == "TL5") {
    # remove taxa not included in SPEAR
    data$scoringTaxaLog[is.na(data$SPEAR_SPECIES)] <- 0
  }
  # calculate sum of Log10 of the abundance for each scoring taxa
  sample_id <- column_attributes$name[1]
  scores <- data %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(
      "abundanceLogSumScoring" =
        sum(scoringTaxaLog[SPEAR_SPECIES == "TRUE"], na.rm = TRUE),
      "abundanceLogSumNonScoring" = sum(scoringTaxaLog),
      "numberOfTaxa" = length(column_attributes$name[3][!is.na(SPEAR_SPECIES)])
    )

  scores <- scores %>% dplyr::mutate(
    "SPEAR ratio" = (numberOfTaxa * abundanceLogSumScoring) /
      (numberOfTaxa * abundanceLogSumNonScoring) * 100
  )

  toxicantExposureCoeff <-
    data.frame(
      recoveryArea = c("absent", "presence", "unknown"),
      p1 = c(1 / -8.02, 1 / -6.16, 1 / -7),
      p2 = c(-1.28 / -8.02, -20.07 / -6.16, -10.675 / -7)
    )
  # toxicant exposure, depending on availability of Recovery area
  # information
  scores <- dplyr::mutate(scores,
    "SPEAR toxic ratio" =
      toxicantExposureCoeff$p1[
        toxicantExposureCoeff$recoveryArea == recoveryArea
      ] *
        `SPEAR ratio` + toxicantExposureCoeff$p2[
          toxicantExposureCoeff$recoveryArea == recoveryArea
        ]
  )
  # SPEAR water quality classes
  # Bad:      <= 11% SPEAR
  # Poor:     > 11% and <= 22% SPEAR
  # Moderate: > 22% and <= 33% SPEAR
  # Good:     > 33% and <= 44% SPEAR
  # High:     > 44% SPEAR
  spearWaterQuality <-
    data.frame(
      "SPEAR class" = c("Bad", "Poor", "Moderate", "Good", "High"),
      upperlimit = c(11, 22, 33, 44, 100),
      value = seq(1:5), check.names = FALSE
    )
  # calculate SPEAR condition using spearWaterQuality dataframe
  intervals <-
    tibble::tibble(value = cut(
      scores$`SPEAR ratio`,
      breaks = c(0, spearWaterQuality[, "upperlimit"]),
      include.lowest = TRUE
    ))
  intervals$score <- as.numeric(intervals$value)
  intervals$row <- row.names(intervals)
  # merge condition lookup table with results
  intervalCondition <-
    merge(spearWaterQuality,
      intervals,
      by.x = "value",
      by.y = "score"
    )
  scores$`SPEAR class` <- intervalCondition$`SPEAR class`
  # create list of the three results: SPEAR score, SPEAR condition, SPEAR
  # class
  scores <- dplyr::select(
    scores,
    sample_id,
    `SPEAR ratio`,
    `SPEAR toxic ratio`,
    `SPEAR class`
  )
  scores <- dplyr::mutate_all(scores, as.character)
  scores <- tidyr::pivot_longer(scores,
    cols = -sample_id,
    names_to = column_attributes$name[2],
    values_to = column_attributes$name[3]
  )
  # need to identify SPEAR outputs by the Taxa list to separate TL5 and TL2
  # outputs
  question <- column_attributes$name[2]
  scores <- mutate(scores, question = paste(question, taxa_list))
  scores <- mutate(scores,
                   !!column_attributes$name[5] := "SPEAR Metric",
                   !!column_attributes$name[6] := "SPEcies At Risk (SPEAR)")

  return(scores)
}
