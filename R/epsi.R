epsi <- function(data,
                 names = macroinvertebrateMetrics::column_attributes$name,
                 questions = c(
                   "Taxon abundance",
                   "Taxon Abundance"
                 ),
                 taxa_list = "TL2",
                 log_abundance = TRUE,
                 metric_cols = metric_cols) {
  if (!taxa_list %in% c("TL2", "TL5")) {
    stop("taxa_list argument must be either 'TL2' or 'TL5'")
  }

  # split by sample number
  sampleMetric <-
    lapply(split(data, data$sample_id), function(sample) {
      # calculate PSI score
      if (log_abundance == FALSE) {
        sample$CAT <- floor(log10(as.numeric(sample$response)) + 1)
      } else {
        sample$CAT <- as.numeric(sample$response)
      }
      if (taxa_list == "TL2") {
        # if no scoring families present return error
        if (sum(sample$EPSI_WEIGHT_FAM, na.rm = TRUE) == 0) {
          samplePsi <- data.frame(
            sample_id = unique(sample$sample_id),
            parameter_long = paste0("Enhanced Proportion of Sediment-sensitive InvertsEPSI Metric ", taxa_list),
            parameter = paste0("EPSI Metric ", taxa_list),
            question = paste0("Error"),
            response = "No EPSI scoring families in sample"
          )
          return(samplePsi)
        }
        sample$PSI_VALUE <- sample$CAT * sample$EPSI_WEIGHT_FAM
        sample$PSI_SENSITIVE_SUM <- sum(sample$PSI_VALUE[sample$EPSI_WEIGHT_FAM >= 0.5], na.rm = TRUE)
      } else {
        sample$PSI_VALUE <- sample$CAT * sample$EPSI_WEIGHT_TL5
        sample$PSI_SENSITIVE_SUM <- sum(sample$PSI_VALUE[sample$EPSI_WEIGHT_TL5 >= 0.5], na.rm = TRUE)
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
        sample_id = unique(sample$sample_id),
        parameter_long = paste0("Enhanced Proportion of Sediment-sensitive Inverts"),
        parameter = paste0("METRIC EPSI"),
        question = c(paste0("EPSI Score ", taxa_list), paste0("EPSI Condition ", taxa_list)),
        response = psiResult
      )

      return(samplePsi)
    })
  output <- do.call("rbind", sampleMetric)
  output <- tibble::as_tibble(output)
  return(output)
}
