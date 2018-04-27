#' @title Standardise Raw Taxa
#'
#' @description
#' This function standardise taxa retrieved from NEMS to all available Taxon Levels.
#' Most of the times this function will be run to generate the input data required
#' by the other functions present in this package.
#'
#' @name StandardiseRawTaxa
#' @aliases StandardiseRawTaxa
#' @usage StandardiseRawTaxa(family.df = NULL, species.df = NULL,
#'                    infer.lower.levels = FALSE,
#'                    aggregate = "sample")
#'
#' @param family.df An optional data frame containing family-level data from
#' NEMS (Analysis: FW_TAX_ID) with at least six columns and in the specified
#' order. Extra columns can be added, but they must be only related to
#' Sampling Point. See 'Details'.
#' @param species.df An optional data frame containing species-level data from
#' NEMS (Analysis: MIXTAX_TST) with at least six columns and in the specified
#' order. Extra columns can be added, but they must be only related to
#' Sampling Point. See 'Details'.
#' @param aggregate Indicates whether the aggregation is either done by \code{"sample"}
#' or by \code{"season"}. Depending on the aggregation, input data frames should be
#' composed of different type of columns.
#' @param infer.lower.levels Flag (\samp{TRUE}/\samp{FALSE}) to infer Taxonomic
#' Levels at TL1, TL2 and TL3 from species-level data.
#' @details
#' Both \code{family.df} and \code{species.df} are data frames containing at least
#' the following six columns, depending on the 'aggregate' parameter:\cr
#' \samp{"season"}: \code{Site}, \code{Season} (integer), \code{Year},
#' \code{Maitland Code}, \code{Maitland Name}, \code{Abundance}.\cr
#' \samp{"sample"}: \code{Site}, \code{Sample Number}, \code{Sample Date} (Date object),
#' \code{Maitland Code}, \code{Maitland Name}, \code{Abundance}.\cr
#' Actually, the routine performs the same calculations for both types of aggregation,
#' the only change is the name of the output data frame's columns.
#'
#' The available codes for \code{Season} and a general description of Taxonomic
#' Levels are defined in the main help page of the \code{macroinvertebrateMetrics} package.
#' @return
#' A list containing 3 data frames.
#'
#'  \code{standard.taxa}: Dataframe containing the following columns (depending on input data):
#'    \code{TL} (integer), \code{Sampling Point}, \code{Season} or \code{Sample},
#'    \code{Year} or \code{Date}, \code{Maitland Code},
#'    \code{Name}, \code{Abundance}, \code{Infered} (integer).\cr
#'    The column \code{Infered} indicates whether TL1/TL2/TL3 were infered from
#'    data in \code{species.df} If the user entered extra columns within the
#'    input dataframes they will be included here as well.
#'
#'  \code{not.included.families}: Dataframe (which can be empty) containing those
#'    input families that have not been found in any of the TL lists.
#'
#'  \code{not.included.species}: Dataframe (which can be empty) containing those
#'    input species that have not been found in any of the TL lists.
#' @note
#' More extra columns can be added to aggregate data by different criteria, i.e.,
#' other than season/date. For doing so you must fill all season/date fields with a dummy value: 1.
#' Thus, the aggregation would disregard season/date as an aggregation column.
#' @examples
#' \dontrun{
#' #No examples yet
#' }
#' @export
######################################################################################
#                                                                                    #
# Version:  1.6                                                                      #
# Revision: 0 - 03/04/2014. Published version                                        #
#           1 - 09/04/2014. Changed aggregation when infering lower levels           #
#           2 - 23/04/2014. Fixed artifical taxa aggregation                         #
#           3 - 17/06/2014. Added backticks to variable names used in formulae       #
#           4 - 14/11/2014. Changes in Maitland codes and taxa grouping logic        #
#           5 - 10/02/2015. NEMS lists changed. Refactoring pending.                 #
#           6 - 24/02/2017. Fixed initial check of (same) number of columns          #
#                                                                                    #
######################################################################################
StandardiseRawTaxa <- function (family.df=NULL, species.df=NULL,
                                infer.lower.levels=FALSE,
                                aggregate="sample") {

  # Input handling
  if (is.null(species.df) && infer.lower.levels == TRUE)
    stop("The species data frame is required for infering lower levels")

  if (is.null(species.df) && is.null(family.df))
    stop("At least either species or family data frame is required")

  if ((!is.null(species.df) && !is.null(family.df)) && length(species.df) != length(family.df))
    stop("Both species and family data frames must have the same number of columns")

  if(class(aggregate) != "character")
    stop("Input aggregate must be a string")

  if (any(!aggregate %in% c("season", "sample")))
    stop("Aggregation level is not valid, please verify its value(s)")

  my.names <- switch(aggregate,
                     season = { c("Site", "Season", "Year", "Code", "Name", "Abun") },
                     sample = { c("Site", "Sample", "Date", "Code", "Name", "Abun") }
                     )
  name.family.cols <- c("TL1 FAMILY", "TL2 FAMILY", "TL3 FAMILY")
  name.species.cols <- c("TL4 TAXON", "TL5 TAXON")

  # --------- Standardise family taxa ---------

  if (!is.null(family.df)) {

    # Change names temporarily to ease working with columns.
    headers.family.df <- names(family.df)[1:6]
    names(family.df)[1:6] <- my.names

    # Count additional columns besides the compulsory 6
    extra.columns <- ncol(family.df) - 6

    # The next family.rows will be useful when checking families without relation
    # Retrieve which family.df-rows are at TL1, TL2 or TL3
    family.rows.at.TL <- lapply(name.family.cols,
                                function (i) {
                                  family.df$Name %in% TaxaList[TaxaList[i] != "N",
                                                                "Taxon name"]
                                })


    # Retrieve and store taxa (using the rows calculated previously)
    # Remember: 3 family levels,  and store it as dataframe.
    family.taxa <- do.call(rbind,
                           lapply(lapply(1L:3L, function(i) {
                             # if there're records at this TL...
                             if (sum(family.rows.at.TL[[i]]) > 0){
                               cbind(TL=i,
                                     family.df[family.rows.at.TL[[i]], ])
                             } else { data.frame() } # empty df
                           }
                           ),
                           data.frame, row.names=NULL, stringsAsFactors=FALSE)
    )

    # Change Maitland Code and Name for those in the TL lists (parent codes)
    for (i in 1L:3L) {
      change.cols <- c(paste0("TL", i, " CODE"), paste0("TL", i, " FAMILY"))
      tmp.TLs <- TaxaList[c("Taxon name", change.cols)]
      family.taxa[family.taxa$TL == i, c("Code","Name")] <-
        tmp.TLs[match(family.taxa[family.taxa$TL == i, "Name"], tmp.TLs[, 1]),
                change.cols]
    }

    # Save all those families that cannot be related to any TL in any way
    family.not.in.TLs <- family.df[!family.rows.at.TL[[1]] & !family.rows.at.TL[[2]]
                                   & !family.rows.at.TL[[3]], ]

  }

  # --------- Open species taxa lists & standardise species ---------

  if (!is.null(species.df)) {

    # Count additional columns besides the compulsory 6
    extra.columns <- ncol(species.df) - 6

    headers.species.df <- names(species.df)[1:6]
    names(species.df)[1:6] <- my.names

    # Next species.rows will be useful when checking species
    # that don't score against any TL
    # Retrieve which species.df-rows are at TL4 and TL5
    species.rows.at.TL <- lapply(name.species.cols,
                                 function(i) {
                                   if (i %in% colnames(TaxaList)) {
                                     species.df$Name %in% TaxaList[TaxaList[i] != "N",
                                                                    "Taxon name"]
                                   } else { NULL }
                                 })

    # Retrieve and store taxa (using the rows calculated previously)
    # Remember: 2 species levels, and store it as dataframe.
    species.taxa <- do.call(rbind,
                            lapply(lapply(1L:2L, function(i) {
                              # if there's records at this TL...
                              if (sum(species.rows.at.TL[[i]]) > 0){
                                cbind(TL=i + 3L,
                                      species.df[species.rows.at.TL[[i]], ])
                              } else { data.frame() } # empty df
                            }
                            ),
                            data.frame, row.names=NULL, stringsAsFactors=FALSE)
    )

    # Change Maitland Code and Name for those in the TL lists (parent codes)
    for (i in 4L:5L) {
      change.cols <- c(paste0("TL", i, " CODE"), paste0("TL", i, " TAXON"))
      if(all(change.cols %in% colnames(TaxaList))) {
        tmp.TLs <- TaxaList[c("Taxon name", change.cols)]
        species.taxa[species.taxa$TL == i, c("Code","Name")] <-
          tmp.TLs[match(species.taxa[species.taxa$TL == i, "Name"],
                        tmp.TLs[, 1]), change.cols]
      }
    }

    # Save all those species that cannot be related to any TL in any way
    # Remember: there can be 2 TLs (TL4 & TL5)
    if (is.null(species.rows.at.TL[[1]])) {

      species.not.in.TLs <- species.df[!species.rows.at.TL[[2]], ]

    } else {

      species.not.in.TLs <- species.df[!species.rows.at.TL[[1]]
                                       & !species.rows.at.TL[[2]], ]
    }

  }

  # --------- Create standardised.taxa -----------

  # Create the standardised.taxa data frame
  if (!is.null(species.df) && !is.null(family.df)) { #none of them is is.null

    standardised.taxa <- rbind(family.taxa, species.taxa)
    rm(family.taxa) #remove some variables

  } else if (is.null(species.df)) { # there is only family.df

    standardised.taxa <- family.taxa
    rm(family.taxa)

  } else { # there is only species.df
    standardised.taxa <- species.taxa

  }
  standardised.taxa$Infered <- 0L

  # --------- Infer.lower.levels ---------

  if (infer.lower.levels) {

    # Infer familiy taxa at TL1, TL2 & TL3 from species

    # Retrieve which species.df-rows are at TL1-3
    species.rows.at.TL13 <- lapply(name.family.cols,
                                   function(i) {
                                     species.df$Name %in% TaxaList[TaxaList[i] != "N",
                                                                    "Taxon name"]
                                   })

    # Retrieve and store taxa (using the rows calculated previously)
    tmp.df <- do.call(rbind,
                      lapply(lapply(1L:3L, function(i) {
                        # if there's records at this TL...
                        if (sum(species.rows.at.TL13[[i]]) > 0){
                          cbind(TL=i,
                                species.df[species.rows.at.TL13[[i]], ])
                        } else { data.frame() } # empty df
                      }),
                      data.frame, row.names=NULL, stringsAsFactors=FALSE)
    )

    # Change Maitland Code and Name for those in the TL lists (parent codes)
    for (i in 1L:3L) {
      change.cols <- c(paste0("TL", i, " CODE"), paste0("TL", i, " FAMILY"))
      tmp.TLs <- TaxaList[c("Taxon name", change.cols)]
      tmp.df[tmp.df$TL == i, c("Code","Name")] <-
        tmp.TLs[match(tmp.df[tmp.df$TL == i, "Name"], tmp.TLs[, 1]), change.cols]
    }

    headers.tmp <- names(tmp.df)

    # Some cleanup of variables
    #rm(species.taxa)

    if (extra.columns > 0) {

      # indices in tmp.df for the additional columns
      extra.inds <- 8:(7 + extra.columns)

      # Indices for those extra columns that only contain NAs;
      # those columns may lead to problems when aggregating or merging dfs.
      # Thus, they will added after the aggregation step
      NAcols <- setdiff(sapply(extra.inds,
                               function(x) x * all(is.na(tmp.df[, x]))), 0)

      # Flag used to check whether there are NAcols
      len.NAcols <- length(NAcols)

      # Indices for those extra columns that do not contain only NAs
      cols <- setdiff(extra.inds, NAcols)
      len.cols <-  length(cols)

      # List of columns used to aggregate the data: TL, and 5 elments of input data
      # plus those extra columns
      if (len.cols > 0) {
        factors <- sapply(c("TL", head(my.names, 5),
                            sapply (cols, function (x) headers.tmp[x])),
                          function (i) paste0("tmp.df$`", i, "`"))
      } else {
        factors <- sapply(c("TL", head(my.names,5)),
                          function (i) paste0("tmp.df$`", i, "`"))

      }

      # Create the aggregation formula
      frm <- as.formula(paste0("tmp.df$Abun ~ ", paste(factors,collapse="+")))

      # Create a df with aggregated data (for family taxa)
      infer.agg <- setNames(aggregate(formula=frm, data=tmp.df, FUN=sum),
                            c(names(factors), "Abun"))


      # Now fill all those columns that are NAs
      if (len.NAcols > 0) {

        sapply (1:len.NAcols, function (x) infer.agg[headers.tmp[NAcols[x]]] <<- NA)

      }

    } else { # no extra columns

      # List of columns used to aggregate the data: TL plus 5 elements of input data
      factors <- sapply(c("TL", head(my.names, 5)),
                        function (i) paste0("tmp.df$`", i, "`"))

      # Create the aggregation formula
      frm <- as.formula(paste0("tmp.df$Abun ~ ", paste(factors,collapse="+")))

      # Create a df with aggregated data (for family taxa)
      infer.agg <- setNames(aggregate(formula=frm, data=tmp.df, FUN=sum),
                            c(names(factors), "Abun"))

    }

    # Create the field Infered (TL5 is the only species TL available in TaxaList)
    infer.agg$Infered <- 5L

    # Combine standardise.taxa and infer.agg
    infer.agg <- infer.agg[, names(standardised.taxa)]
    standardised.taxa <- rbind(standardised.taxa, infer.agg)
    rm(infer.agg, tmp.df)

  } else { # infer.lower.levels == FALSE

    # Some cleanup of variables
    if (!is.null(species.df)) {
      rm(species.taxa)
    }

  }

  standard.names <- names(standardised.taxa)

  # --------- Finally, aggregate values ------

  # store the factors of the aggregation formula:
  if (extra.columns > 0) {

    # indices in standardised.taxa for the additional columns
    extra.inds <- 8:(7 + extra.columns)

    # Indices for those extra columns that only contain NAs;
    # those columns may lead to problems when aggregating or merging dfs.
    # Thus, they will added after the aggregation step. Possibly any column
    # containing NAs would give problem, we would be better working with
    # 'unique' and 'aggregation'.
    NAcols <- setdiff(sapply(extra.inds,
                             function(x) x * all(is.na(standardised.taxa[, x]))), 0)

    # Flag used to check whether there are NAcols
    len.NAcols <- length(NAcols)

    # Indices for those extra columns that do not contain only NAs
    cols <- setdiff(extra.inds, NAcols)
    len.cols <-  length(cols)


    unique.taxa <- unique(standardised.taxa[, c(1:6, 8:ncol(standardised.taxa))])


    if (len.cols > 0) {
      factors <- sapply(c("TL", head(my.names, 3), "Name",
                          sapply (cols, function (x) standard.names[x]),
                          "Infered"),
                        function (i) paste0("standardised.taxa$`", i, "`"))
    } else {
      factors <- sapply(c("TL", head(my.names, 3), "Name", "Infered"),
                        function (i) paste0("standardised.taxa$`", i , "`"))

    }


    # Create the aggregation formula
    frm <- as.formula(paste0("standardised.taxa$Abun ~ ",
                             paste(factors, collapse="+")))

    # Modify dataframe with aggregated data, using merge too.
    # When aggregating data by (one or) several columns and you also want to keep
    # several other columns in the resulting data frame, a combination of 'merge'
    # and 'aggregate' comes handy.
    standardised.taxa <- merge(setNames(aggregate(formula=frm,
                                                  data=standardised.taxa, FUN=sum),
                                        c(names(factors), "Abun")), unique.taxa)

    # Reorder columns
    standardised.taxa <- standardised.taxa[, standard.names]
    standardised.taxa <- standardised.taxa[, c(1:7, ncol(standardised.taxa), extra.inds)]

  } else { # extra.columns = 0

    unique.taxa <- unique(standardised.taxa[, c(1:6, 8:ncol(standardised.taxa))])

    factors <- sapply(c("TL", head(my.names, 3), "Name", "Infered"),
                      function (i) paste0("standardised.taxa$`", i, "`"))

    # Create the aggregation formula
    frm <- as.formula(paste0("standardised.taxa$Abun ~ ",
                             paste(factors,collapse="+")))

    standardised.taxa <- merge(setNames(aggregate(formula=frm, data=standardised.taxa,
                                                  FUN=sum),c(names(factors), "Abun")),
                               unique.taxa)

    # Reorder columns
    standardised.taxa <- standardised.taxa[, standard.names]

  }

  # --------- Return values ---------

  # Return a list with standardised.taxa and those families and species that
  # couldn't be standardised.

  if (is.null(species.df)) {

    out <- list(standard.taxa=standardised.taxa,
                not.included.families=family.not.in.TLs,
                not.included.species=data.frame())

  } else if ((is.null(family.df))) {

    out <- list(standard.taxa=standardised.taxa,
                not.included.families=data.frame(),
                not.included.species=species.not.in.TLs)

  } else {

    out <- list(standard.taxa=standardised.taxa,
                not.included.families=family.not.in.TLs,
                not.included.species=species.not.in.TLs)
  }

} # end StandardiseRawTaxa
