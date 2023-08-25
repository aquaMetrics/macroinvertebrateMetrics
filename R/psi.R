#' Proportion of Sediment-sensitive Invertebrates (PSI)
#'
#' A sediment-sensitive macro-invertebrate metric that provides a proxy to
#' describe the extent to which the surface of river bed are composed, or
#' covered by sediments. It can be calculated at Taxonomic Levels 3, 4 & 5
#' @param data
#' Dataframe with at least three columns
#' item{sample_id} - unique idenftier for each sample
#' item{label} - Taxon name that matches to macroinvertebrateTaxa dataset
#' item{response} - Log abundance category
#' @param taxa_list The taxonomic level the sample(s) have been identified at
#' according to specificed taxa lists as described in WFD100 Further
#' Development of River Invertebrate Classification Tool. Either "TL3" - Taxa
#' list 3, "TL4" - Taxa list 4 or  "TL5" - Taxa list 5.
#' @return Dataframe with
#' item{sample_id}
#' item{parmaeter}
#' item{question}
#' item{response}
#' @references
#' Extence, Chris & Chadd, Richard & England, Judy & Dunbar, M.J. & Wood, Paul &
#' Taylor, E.D.. (2010). The Assessment of Fine Sediment Accumulation in Rivers
#' Using Macro-Invertebrate Community Response. River Research and Applications.
#' 29. 10.1002/rra.1569.
psi <- function(data, taxa_list = "TL3") {
  if (!taxa_list %in% c("TL3", "TL5", "TL4")) {
    stop("taxaList arugment must be either 'TL3', 'TL4' or 'TL5'")
  }
  # unique taxa incase any duplicate taxa causes double counting. Metric is
  # based on present / absent so don't need to group by abundance etc.
  data <- unique(data[, c("sample_id", "label", "response")])

  # merge ecology results with taxa metric scores based on taxon name
  macroinvertebrates <- macroinvertebrateMetrics::macroinvertebrateTaxa
  data <-
    merge(data,
      macroinvertebrates,
      by.x = "label",
      by.y = "TAXON_NAME"
    )
  psiSensitivity_score <- macroinvertebrateMetrics::psiSensitivityScore
  data <-
    merge(data,
      psiSensitivity_score,
      by.x = c("PSI_GROUP", "response"),
      by.y = c("GROUP", "LOG10.ABUN.CAT")
    )

  # split by sample number
  sample_metrics <-
    lapply(split(data, data$sample_id), function(sample) {
      # calculate PSI score
      sample_metric <-
        sum(sample$SEDIMENT.SS[sample$PSI_GROUP %in% c("A", "B")], na.rm = TRUE) /
          sum(sample$SEDIMENT.SS[sample$SEDIMENT.SS != "" |
            is.na(sample$SEDIMENT.SS)], na.rm = TRUE) * 100
      # calculate PSI condition using psiCondition dataframe saved in package
      psi_conditions <- macroinvertebrateMetrics::psiCondition
      intervals <-
        data.frame(value = table(cut(
          sample_metric,
          breaks = c(0, psi_conditions[, "upperlimit"]),
          include.lowest = TRUE
        )))
      intervals$row <- row.names(intervals)
      # merge condition lookup table with results
      interval_condition <-
        merge(psi_conditions, intervals, by.x = "value", by.y = "row")
      # create list of psi score and psi condition
      psi_result <- c(
        sample_metric,
        as.character(interval_condition$condition[
          interval_condition$value.Freq == 1
        ])
      )
      # create dataframe of results
      sample_psi <- data.frame(
        sample_id = unique(sample$sample_id),
        parameter_long = paste0("Proportion of Sediment-sensitive Inverts"),
        parameter = paste0("METRIC PSI"),
        question = c(
          paste0("PSI Score ", taxa_list),
          paste0("PSI Condition ", taxa_list)
        ),
        response = psi_result
      )

      return(sample_psi)
    })
  metric <- do.call("rbind", sample_metrics)
  metric <- tibble::as_tibble(metric)
  return(metric)
}
