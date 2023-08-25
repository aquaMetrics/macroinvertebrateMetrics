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
#' @export
#'
#' @examples
#' sample <- demo_data
#' calc_psi(data = sample, taxa_list = "TL3")
calc_psi <- function(data,
                     names = macroinvertebrateMetrics::column_attributes$name,
                     questions = c(
                       "Taxon abundance",
                       "Taxon Abundance"
                     ),
                     taxa_list = "TL3",
                     log_abundance = TRUE,
                     metric_cols = macroinvertebrateMetrics::metric_cols) {
  metric <- calc_metric(data,
    metrics = "psi",
    taxa_list = taxa_list,
    names = names,
    questions = questions,
    metric_cols = metric_cols,
    log_abundance = log_abundance
  )

  return(metric)
}
