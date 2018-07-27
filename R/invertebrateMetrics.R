#' @title Freshwater Biotic indices
#'
#' @description
#' Metrics for assessing ecological indicators of biodiversity in rivers and lakes.
#' Specifically coded to be used with TIBCO Enterprise Runtime for R (TERR).
#'
#' @name invertebrateMetrics
#' @aliases invertebrateMetrics
#' @docType package
#' @details
#' \tabular{ll}{
#'  Package: \tab invertebrateMetrics\cr
#'  Type: \tab Package\cr
#'  Version: \tab 0.98-2\cr
#'  Date: \tab 2017-03-30\cr
#'  License: \tab MIT\cr
#' }
#'
#' This package contains several functions which compute multiple ecological
#' metrics at different Taxonomic Levels (TL) using multiple season combinations.
#' \itemize{
#'  \item SPEAR (\code{\link{calcSpear}})\cr Species at Risk
#'  \item PSI (\code{\link{calcPsi}})\cr Proportion of Sediment-sensitive Invertebrates
#'  \item LIFE (\code{\link{CalcLIFE}})\cr Lotic Invertebrate Index for Flow Evaluation
#'  \item Riverfly's ASI (\code{\link{CalcASI}})\cr Riverfly Angler's Score Index
#'  \item Climate Change (\code{\link{CalcCC}})\cr Climate Change. Thermal indicator.
#' }
#'
#' Taxonomic Levels are the different output options developed by RIVAPCS
#' to produce results at both species and family levels:\cr
#' TL1 - BMWP families\cr
#' TL2 - Revised BMWP (WHPT) families\cr
#' TL3 - All families\cr
#' TL4 - RIVPACS species\cr
#' TL5 - WFD species
#'
#' Season codes define different season combinations used to calculate the
#' indices, those are listed below:\cr
#' \samp{1} = spring\cr
#' \samp{2} = summer\cr
#' \samp{3} = autumn\cr
#' \samp{4} = spring + summer\cr
#' \samp{5} = spring + autumn\cr
#' \samp{6} = summer + autumn\cr
#' \samp{7} = spring + summer + autumn
#'
#' There is also a common routine (\code{\link{StandardiseRawTaxa}}) which should
#' be run prior to any calculation in order to standardise taxa counts into the
#' required Taxonomic Levels.
#'
#' @author Scottish Environment Protection Agency
#'
#' Maintainer: Carlos Ruiz \email{carlos.ruiz@@sepa.org.uk}
#'
#' @keywords package
NULL

#' Taxa lists from NEMS
#'
#' Lookup table of taxa found in NEMS at both family and species level.
#'
#' The columns are as follows:
#'
#' \itemize{
#'  \item Taxon name.
#'  \item TAXON GROUP.
#'  \item FAMILY.
#'  \item MAITLAND CODE.
#'  \item ARTIFICIAL TAXON.
#'  \item Metrics Scores (LIFE GROUP, LIFE COMPOSITE, PSI GROUP, SPEAR SPECIES).
#'  \item Taxons (TL5 TAXON, TL3 FAMILY, TL2 FAMILY, TL1 FAMILY).
#'  \item MAITLAND_CODE (including leading 0, up to 8 digits).
#'  \item TL codes (TL5 CODE, TL3 CODE, TL2 CODE, TL1 CODE)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name TaxaList
#' @format A data frame with 6491 rows and 18 variables
NULL

#' PSI. Fine Sediment Sensitivity Rating definitons and abundance
#'
#' Fine Sediment Sensitivity Rating definitons and abundance weighted scores for
#' PSI calculation
#'
#' The columns are as follows:
#'
#' \itemize{
#'  \item GROUP. Sediment-sensitivity group.
#'  \item DESCRIPTION.
#'  \item LOG10.ABUN.CAT.  log10(abundance) category
#'  \item SEDIMENT.SS.  Abundance weighted scores
#' }
#'
#' @docType data
#' @keywords datasets
#' @name PSISensitivity
#' @format A data frame with 30 rows and 4 variables
NULL

#' LIFE. Scores of taxa associated with flow groups
#'
#' Scores for different abundance categories of taxa associated with flow groups 1-6
#'
#' The columns are as follows:
#'
#' \itemize{
#'  \item Flow.Group.
#'  \item Flow.Group.Description.
#'  \item Log10.Abundance.Category. Based on log10(abundance).
#'  \item fs.  Taxon flow score.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name LIFEFlowScores
#' @format A data frame with 36 rows and 4 variables
NULL

#' Lookup ASI
#'
#' Lookup table of taxa classification for Riverfly's ASI analysis
#'
#' The columns are as follows:
#'
#' \itemize{
#'  \item TL. Taxonomic level of the taxon.
#'  \item CODE. Maitland Code.
#'  \item NAME.  Name of the taxon.
#'  \item ASI.  Target group
#' }
#'
#' @docType data
#' @keywords datasets
#' @name LookUpASI
#' @format A data frame with 33 rows and 4 variables
NULL

#' Lookup Climate Change
#'
#' Lookup table of taxa classification for Climate Change analysis
#'
#' The columns are as follows:
#'
#' \itemize{
#'  \item TL. Taxonomic level of the taxon.
#'  \item CODE. Maitland Code.
#'  \item NAME.  Name of the taxon.
#'  \item TEMP.  Temperature tolerance
#'  \item NEW.CODE.  New Maitland Code
#' }
#'
#' @docType data
#' @keywords datasets
#' @name LookUpCC
#' @format A data frame with 246 rows and 5 variables
NULL
