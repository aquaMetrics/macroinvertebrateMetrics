#' Calculate Pollution Silt Index (PSI)
#' @param ecologyResults
#' Dataframe of Ecology results
#' @return dataframe with...
#' @export
#'
#' @examples
#' calcEpsi(demoEcologyResults)
calcPsi <- function(ecologyResults) {
  # split by sample number
  sampleMetric <- lapply(split(ecologyResults, ecologyResults$SAMPLE_NUMBER), function(sample) {
  # calculate PSI score
   sampleMetric <- length(sample$PSI_GROUP[sample$PSI_GROUP %in% c("A", "B")]) /
     length(sample$PSI_GROUP[!is.na(sample$PSI_GROUP)]) * 100
  # calculate PSI condition using psiCondition dataframe saved in package
  intervals <- data.frame(value = table(cut(sampleMetric, breaks = c(0, psiCondition[, "upperlimit"]),
                                              include.lowest = TRUE)))
  intervals$row <- row.names(intervals)
  # merge condition lookup table with results
  intervalCondition <- merge(psiCondition, intervals, by.x = "value", by.y = "row")
  # create list of psi score and psi condition
  psiResult <- c(sampleMetric, as.character(intervalCondition$condition[intervalCondition$value.Freq == 1]))
  # create dataframe of results
  samplePsi <- data.frame(SAMPLE_NUMBER = unique(sample$SAMPLE_NUMBER),
                           DETERMINAND = c("PSI Score", "PSI Condition"),
                           RESULT = psiResult)
  })
  metric <- do.call("rbind", sampleMetric)
  return(metric)
}