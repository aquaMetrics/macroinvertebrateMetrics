#' @title Freshwater Biotic indices for macroinvertebrates
#'
#' @description
#' Metrics for assessing ecological indicators of macroinvertebrates in rivers.
#'
#' @name invertebrateMetrics
#' @aliases invertebrateMetrics
#' @docType package
#' @export
#'
#' This package contains several functions which compute multiple ecological
#' metrics at different Taxonomic Levels (TL) using multiple season combinations.
#' \itemize{
#'  \item SPEAR (\code{\link{calcSpear}})\cr Species at Risk
#'  \item PSI (\code{\link{calcPsi}})\cr Proportion of Sediment-sensitive Invertebrates
#'  \item PSI (\code{\link{calcEpsi}})\cr Empirically-weighted Proportion of Sediment-sensitive Invertebrates
#'  \item Riverfly score (\code{\link{calcRiverfly}})\cr Riverfly Angler's Score Index
#'  \item WHPT (\code{\link{calcWhpt}})\cr WHPT Score Index
#' }
#'
#' Taxonomic Levels are the different output options developed by RIVAPCS
#' to produce results at both species and family levels:\cr
#' TL1 - BMWP families\cr
#' TL2 - Revised BMWP (WHPT) families\cr
#' TL3 - All families\cr
#' TL4 - RIVPACS species\cr
#' TL5 - WFD species
