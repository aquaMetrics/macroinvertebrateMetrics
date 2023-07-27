#' Empirically-weighted Proportion of Sediment-sensitive Invertebrates (E-PSI)
#'
#' A sediment-sensitive macro-invertebrate metric that provides a proxy to
#' describe the extent to which the surface of river bed are composed, or
#' covered by sediments. It can be calculated at Taxonomic Levels 2 & 5
#' @param ecologyResults
#' Dataframe with at least three columns
#' item{SAMPLE_ID} - unique idenftier for each sample
#' item{TAXON} - Taxon name that matches to macroinvertebrateTaxa dataset
#' item{RESULT} - Numeric log abundance category
#' @param taxaList The taxonomic level the sample(s) have been identified at
#' according to specificed taxa lists as described in WFD100 Further
#' Development of River Invertebrate Classification Tool. Either "TL2" - Taxa
#' or  "TL5" - Taxa list 5.
#' @param logAbundance If RESULT column in ecologyResults not log category then
#' set to FALSE. This will calculate log value from your numeric abundance.#'
#' @return Dataframe with
#' item{SAMPLE_ID}
#' item{ANALYSIS_NAME}
#' item{DETERMINAND}
#' item{RESULT}
#' @references
#' Turley, Matt & Bilotta, Gary & Chadd, Richard & A Extence, Chris & E Brazier,
#' Richard & Burnside, Niall & Pickwell, Alex. (2016). A sediment-specific
#' family-level biomonitoring tool to identify the impacts of fine sediment in
#' temperate rivers and streams. Ecological Indicators. 70. 151-165.
#' 10.1016/j.ecolind.2016.05.040.
#' @export
#'
#' @examples
#' sample <- demoEcologyResults
#' sample <- filter_psi(sample, taxaList = "TL2")
#' calc_epsi(ecologyResults = sample)
calc_epsi <- function(ecologyResults, taxaList = "TL2", logAbundance = TRUE) {
  if (!taxaList %in% c("TL2", "TL5")) {
    stop("taxaList argument must be either 'TL2' or 'TL5'")
  }
  # merge ecology results with taxa metric scores based on taxon name
  macroinvertebrates <- macroinvertebrateMetrics::macroinvertebrateTaxa
  ecologyResults <-
    merge(ecologyResults,
      macroinvertebrates,
      by.x = "TAXON",
      by.y = "TAXON_NAME"
    )

  # split by sample number
  sampleMetric <-
    lapply(split(ecologyResults, ecologyResults$SAMPLE_ID), function(sample) {
      # calculate PSI score
      if (logAbundance == FALSE) {
        sample$CAT <- floor(log10(sample$RESULT) + 1)
      } else {
        sample$CAT <- sample$RESULT
      }
      if (taxaList == "TL2") {
        # if no scoring families present return error
        if (sum(sample$EPSI_WEIGHT_FAM, na.rm = TRUE) == 0) {
          samplePsi <- data.frame(
            SAMPLE_ID = unique(sample$SAMPLE_ID),
            ANALYSIS_REPNAME = paste0("Enhanced Proportion of Sediment-sensitive InvertsEPSI Metric ", taxaList),
            ANALYSIS_NAME = paste0("EPSI Metric ", taxaList),
            DETERMINAND = paste0("Error"),
            RESULT = "No EPSI scoring families in sample"
          )
          return(samplePsi)
        }
        sample$PSI_VALUE <- sample$CAT * sample$EPSI_WEIGHT_FAM
        sample$PSI_SENSITIVE_SUM <- sum(sample$PSI_VALUE[sample$EPSI_WEIGHT_FAM >= 0.5], na.rm = T)
      } else {
        sample$PSI_VALUE <- sample$CAT * sample$EPSI_WEIGHT_TL5
        sample$PSI_SENSITIVE_SUM <- sum(sample$PSI_VALUE[sample$EPSI_WEIGHT_TL5 >= 0.5], na.rm = T)
      }

      sample$PSI_ALL_SUM <- sum(sample$PSI_VALUE, na.rm = TRUE)
      sample$PSI_SCORE <- (sample$PSI_SENSITIVE_SUM / sample$PSI_ALL_SUM) * 100
      sampleMetric <- unique(sample$PSI_SCORE)
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
      psiResult <- c(
        sampleMetric,
        as.character(intervalCondition$condition[intervalCondition$value.Freq == 1])
      )
      # create dataframe of results
      samplePsi <- data.frame(
        SAMPLE_ID = unique(sample$SAMPLE_ID),
        ANALYSIS_REPNAME = paste0("Enhanced Proportion of Sediment-sensitive Inverts"),
        ANALYSIS_NAME = paste0("METRIC EPSI"),
        DETERMINAND = c(paste0("EPSI Score ", taxaList), paste0("EPSI Condition ", taxaList)),
        RESULT = psiResult
      )

      return(samplePsi)
    })
  metric <- do.call("rbind", sampleMetric)

  return(metric)
}
