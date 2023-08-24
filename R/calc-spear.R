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
#' \code{\link{filterSpear}}
#' @export
#'
#' @examples
#' ecologyResults <- macroinvertebrateMetrics::demoEcologyResults
#' ecologyResults <- ecologyResults[ecologyResults$SAMPLE_ID == 3201863, ]
#' ecologyResults <- ecologyResults[ecologyResults$ANALYSIS_NAME == "FW_TAX_ID", ]
#' sample <- filter_spear(demo_data, taxa_list = "TL2")
#' spearOutput <- calc_spear(sample, taxa_list = "TL2")
#'
calc_spear <- function(data, recoveryArea = "unknown", taxa_list = "TL2") {

  output <- calc_metric(data, metrics = "spear")

}
