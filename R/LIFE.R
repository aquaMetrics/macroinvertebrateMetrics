#' @title Lotic-invertebrate Index for Flow Evaluation
#'
#' @description
#' The LIFE method is based on recognized flow associations of different
#' macroinvertebrate taxa. It links qualitative and semi-quantitative change
#' in riverine benthic macroinvertebrate communities to prevailing flow regimes.
#' It can be calculated at Taxonomic Levels 5, 2 and 1 (for TL1/2 composite
#' families).
#'
#' @name CalcLIFE
#' @aliases CalcLIFE
#' @usage CalcLIFE(data, season = NULL, TL = 2L)
#'
#' @param data A dataframe containing \emph{standardised} taxa with at least
#' eight columns and in the specified order. Extra columns can be added,
#' but they must be related to Sampling Point. See 'Details'
#' @param season Optional. An integer vector containing up to 7 elements,
#' from \samp{1L} to \samp{7L}. It is only needed when the input data frame contains
#' Season and Year instead of Sample ID and Date. See 'Details' for more info about
#' input columns.
#' @param TL An integer vector containing up to 3 elements which would
#' represent the taxonomic level to calulate LIFE at. They can be \samp{1L}
#' (for TL1/2), \samp{2L} and \samp{5L}.
#' @details
#' The input dataframe \code{data} must contain a minimum of eight columns in the
#' specified order: \code{TL} (Taxonomic Level), \code{Site}, \code{Season} or \code{Sample ID},
#' \code{Year}  or \code{Date} (Date object), \code{Maitland Code}, \code{Maitland Name},
#' \code{Abundance}, \code{Infered} (Code returned by the function \code{StandardiseRawTaxa}
#' which can be declared as \samp{0} if \code{StandardiseRawTaxa} is not used).
#'
#' If the data frame contains Season and Year instead of Sample ID / Date, the \code{season}
#' parameter must be a vector of the form \samp{c(1L, 3L, 5L)}, indicating which seasons, and
#' combination of seasons, must be computed.
#'
#' The available codes for \code{Season} and a general description of Taxonomic
#' Levels are defined in the main help page of the \code{aquaMetrics} package.
#' @return
#'  A dataframe with the following columns: \code{TL}, \code{Site},
#' \code{Season} or \code{Sample ID}, \code{Year} or \code{Date} (Date object),
#' \code{Infered}, extra columns - if any, and \code{LIFEScore}.
#' @note
#' More extra columns can be added to aggregate data by different criteria, i.e.,
#' other than season or Sample ID. For doing so you must fill all season fields with a dummy
#' value: \samp{1}.Thus, the aggregation would disregard season/Sample ID as an aggregation column.
#' @references
#' Extence, C, Balbi, D., Chadd, R. 1999. \emph{River flow indexing using
#' British benthic macroinvertebrates: A framework for setting hydroecological
#' objectives}. Regul Rivers Res Mgmt. 15: 543-574.
#' @seealso
#' \code{\link{StandardiseRawTaxa}}
#' @examples
#' \dontrun{
#' #No examples yet
#' }
#' @export
######################################################################################
#                                                                                    #
# Version:  1.2                                                                      #
# Revision: 0 - 28/05/2014. Published version                                        #
#           1 - 10/02/2015. NEMS lists changed. Refactoring pending.                 #
#           2 - 30/03/2017. Fixed bug in checking aggregate method (sample/sesion).  #
#                                                                                    #
######################################################################################
CalcLIFE <- function(data, season=NULL, TL=2L) {

  # Input handling
  if (is.null(data))
    stop("No dataframe has been specified as 'data'")

  if (ncol(data) < 8)
    stop("It seems the input data.frame does not have the required columns")

  if (any(!TL %in% c(1L, 2L, 4L, 5L)))
    stop("Input TL is not valid, please verify its value(s)")

  is.POSIXt <- function(x) inherits(x, "POSIXt")
  is.Date <- function(x) inherits(x, "Date")
  aggregate <- ifelse(sapply(data[4], is.POSIXt) || sapply(data[4], is.Date), "sample", "season")

  if (aggregate == "season" && any(!season %in% 1:7))
    stop("Input season is not valid, please verify its value(s)")

  header.names <- names(data)

  # Make sure we only work with user's input TL
  if (1L %in% TL && !2L %in% TL) {
    # we need TL2 for TL1/2 regardless it is not defined in the input.
    data <- data[data[, 1] %in% TL | data[, 1] == 2L, ]
  } else {
    data <- data[data[, 1] %in% TL, ]
  }

  # Remove abundances being 0 because there are no 'fs' for Log(abundance) = 0
  data <- data[data[, 7] > 0, ]

  num.columns <- ncol(data)

  # Save the indices of the extra columns in the data frame
  if (num.columns > 8) {

    num.extracolumns <- num.columns - 8
    extracolumns.inds <- seq(9, 8 + num.extracolumns)

  } else { #num.columns == 8

    num.extracolumns <- 0
    extracolumns.inds <- 0

  }

  dfrows <- nrow(data)
  # and more columns:
  # Log(1 + Abundance) --and rounded up
  # Group ID --to group rows by TL, season, year, site, and extra columns
  data[c("abLog", "GroupID")] <- rep(NA, dfrows)


  # data.frame to store the results to return by the function
  results <- data.frame()

  #  ----------------------

  # Note that LIFE calculates indices at TL1-2, TL2, 4 & 5.
  taxa.list.levels <- c("TL2 FAMILY", "TL5 TAXON")
  taxa.list.TL2.cols <- c("TL2 CODE", "TL2 FAMILY", "LIFE GROUP", "LIFE COMPOSITE")
  taxa.list.TL5.cols <- c("TL5 CODE", "TL5 TAXON", "LIFE GROUP", "LIFE COMPOSITE")
  taxa.list.cols <- as.data.frame(cbind("2"=taxa.list.TL2.cols, "5"=taxa.list.TL5.cols),
                                  stringsAsFactors=FALSE)
  lifeDB <- do.call(rbind,
                     lapply(lapply(taxa.list.levels, function(i) {
                       taxa.column <- taxa.list.cols[2, substr(i, 3, 3)]
                       res <- cbind(TL=as.integer(substr(i, 3, 3)),
                                TaxaList[TaxaList[taxa.column] != "N" &
                                (TaxaList$`Taxon name` == TaxaList[taxa.column] |
                                 grepl("#", TaxaList$`Taxon name`, fixed=TRUE) |
                                 TaxaList$`Taxon name` ==
                                  sapply(strsplit(as.character(TaxaList[taxa.column][[1]]), " "), `[`, 1)) &
                                 !is.na(TaxaList[taxa.list.cols[3, substr(i, 3, 3)]]),
                                 taxa.list.cols[, substr(i, 3, 3)]]
                              )
                       colnames(res)[2:5] <- c("CODE", "NAME", "LIFE", "COMPOSITE")
                       return(res)
                     }),
                     data.frame, row.names=NULL, stringsAsFactors=FALSE
                     )
            )

  # Bit of a hack, instead of TL=2 we will call it TL=12 (as in 1/2)
  lifeDB[lifeDB$TL==2, "TL"] = 12L

  # Bind extra rows of TL1 taxa for composite TL1/2
  compo.rows <- cbind(TL=12L, TaxaList[TaxaList$`LIFE COMPOSITE` == 'Y',
                  c("TL1 CODE", "Taxon name", "LIFE GROUP", "LIFE COMPOSITE")],
                  row.names=NULL, stringsAsFactors=FALSE)
  colnames(compo.rows)[2:5] <- c("CODE", "NAME", "LIFE", "COMPOSITE")
  lifeDB <- rbind(lifeDB, compo.rows)

  #and swap empty COMPOSITE for "N/A"
  lifeDB[lifeDB$COMPOSITE != "Y" & lifeDB$COMPOSITE != "N", "COMPOSITE"] <- "N/A"

  # Retrieve table with flow scores
  lifeFlowScore <- LIFEFlowScores

  # Combination of seasons
  combined.seasons <- list("4"=1:2, "5"=c(1,3), "6"=2:3, "7"=1:3)

  # Retrieve LIFE groups
  # Merge both lifeDB and data to get the LIFE group. And replace the existing data
  # by the result.
  colnames(lifeDB)[1] <- header.names[1] # Make both names equal
  colnames(lifeDB)[2] <- header.names[5] # Make both names equal
  lifeDB.names <- colnames(lifeDB)
  # In making names used for the merging equal, we are able to maintain the
  # column order in the resulting dframe when using union in the merge.

  # Work with data at TL2 - TL5 and TL1/2 separately
  data25 <- data.frame()
  data12 <- data.frame()

  if (any(TL %in% c(2L, 5L))) {
    lifeDB25 <- lifeDB[lifeDB$COMPOSITE != "Y", ]

    data25 <- data[data[, 1] %in% c(2L, 5L), ]

    data25[data25[1] == 2L, 1] <- 12L  # hack needed for the merge with "TL 1/2" listing

    data25 <- merge(data25, lifeDB25[c(1, 2, 4, 5)], by=c(header.names[1],
                  header.names[5]))[, union(names(data25), names(lifeDB25[c(1, 2, 4, 5)]))]
    data25[data25[1] == 12L, 1] <- 2L # restore TL2 value
  }

  if (1L %in% TL) {
    lifeDB12 <- lifeDB[lifeDB$COMPOSITE != "N", ]

    data12 <- data[data[, 1] == 1 & data[, 5] %in% lifeDB12[lifeDB12$COMPOSITE == "Y", 2], ]
    data12 <- rbind(data12, data[data[, 1] == 2 &
                                   data[, 5] %in% lifeDB12[lifeDB12$COMPOSITE == "N/A", 2], ])

    data12[, 1] <- 12L  # hack needed for the merge with "TL 1/2" listing

    data12 <- merge(data12, lifeDB12[c(1, 2, 4, 5)], by=c(header.names[1],
                    header.names[5]))[, union(names(data12), names(lifeDB12[c(1, 2, 4, 5)]))]
    data12[, 1] <- 1L # save as TL1 (with TL2 families)
  }

  # merge dataframes
  data <- rbind(data12, data25)

  # Fill an ID column which would group items by TL, site, year, season,
  # infered and extra-columns.
  # Hopefully, it will make easier further aggregations and calcs.
  if (num.extracolumns > 0) {
    data$GroupID <- as.numeric(factor(do.call(paste, data[, c(1:4, 8, extracolumns.inds)])))
  } else {
    data$GroupID <- as.numeric(factor(do.call(paste, data[, c(1:4, 8)])))
  }

  # To ease data manipulation, those extra columns in the dataset should be placed
  # at the end, so that we can use the same indices no matter how many extra
  # columns are.
  num.columns <- ncol(data)
  if (num.extracolumns > 0) {
    move.tolast <- names(data[extracolumns.inds])
    data <- data[c(setdiff(names(data), move.tolast), move.tolast)]
    extracolumns.inds <- seq(num.columns - num.extracolumns + 1, num.columns)
  }
  header.names <- names(data)

  # Calculate log10 of Abundance and round it up to the next integer
  data$abLog <- ceiling(log10(data[, 7] + 1))

  # Define Flow scores
  colnames(lifeFlowScore)[1] <- header.names[11] # Make both names equal
  colnames(lifeFlowScore)[3] <- header.names[9] # Make both names equal
  lifeFlowScore.names <- colnames(lifeFlowScore)
  data <- merge(data, lifeFlowScore[, c(1, 3, 4)],
                by=c(header.names[9],header.names[11]),
                all.x=TRUE)[, union(names(data),
                                    names(lifeFlowScore[c(1, 3, 4)]))]
  num.columns <- ncol(data)

  if (aggregate == "season") {

    # We have calculated every season available, but we did not check whether the
    # user wanted to. We must remove those seasons which are not defined in the
    # input "season" parameter, before aggregating data.
    # If there is no single season then we wil have an empty single data.frame
    single.data <- data[data[, 3] %in% season, ]

  } else { single.data <- data }

  if (nrow(single.data) > 0) {

    if (num.extracolumns > 0) {

      # Create 'single.res' data.frame (aggregated results)
      single.res <- unique(single.data[, c(1:4, 8, 10, extracolumns.inds)])

    } else {

      # Create 'single.res' data.frame (aggregated results)
      single.res <- unique(single.data[, c(1:4, 8, 10)])

    }

    # calculate LIFE score per group (GroupID). Only taxa with abundance > 0!
    LIFEScores <- as.data.frame(cbind(LIFEScore=by(single.data, single.data$GroupID,
                    FUN=function(x)
                    sum(x[, "fs"])/nrow(x[x[, 7] > 0, ]))))
    LIFEScores$GroupID <- rownames(LIFEScores)
    rownames(LIFEScores) <- NULL

    # merge LIFE scores to our data frame of results
    single.res <- merge(single.res, LIFEScores, by="GroupID", all.x=TRUE)[-1]

  } else { # there are no single season aggregate data to calculate

    single.res <- data.frame() # empty

  } # end if (nrow(single.data) > 0)


  ### Combined seasons ###

  input.combined.seasons <- season[season %in% 4:7]

  if (length(input.combined.seasons) > 0) { # user input contains combined seasons

    # Initialize the combined seasons result data.frame
    combined.res <- data.frame()

    # for each required combined season:
    for (s in input.combined.seasons) {

      # Select only those rows needed for the combination of the current combination
      combined.data <- data[data[, 3] %in% combined.seasons[[as.character(s)]], ]

      # Reset some values:
      # Remove flow scores
      combined.data$fs <- NULL
      # Reset abLog
      combined.data$abLog <- NA

      # Prepare a minimal.dataframe with common data:
      if (num.extracolumns > 0) {

        # Create a new GroupID as we do not have season here
        combined.data$GroupID <- as.numeric(factor(do.call(
          paste, combined.data[, c(1:2, 4, 8, extracolumns.inds)])))

        tmp.res <- unique(combined.data[, c(1:2, 4, 8, 10, extracolumns.inds)])

      } else {

        combined.data$GroupID <- as.numeric(factor(do.call(
          paste, combined.data[, c(1:2, 4, 8)])))

        tmp.res <- unique(combined.data[, c(1:2, 4, 8, 10)])

      }

      # -------------
      # Aggregate and merge, leaving out "season" for the aggregation
      agg.tmp <- setNames(aggregate(combined.data[, 7] ~
                                      combined.data[, 5] + combined.data$GroupID,
                                    data=combined.data, FUN=sum),
                          c(header.names[5], "GroupID", header.names[7]))

      combined.data <- merge(unique(combined.data[c(1:2, 4:6, 8:ncol(combined.data))]),
                             agg.tmp, by=c(header.names[5], "GroupID"))[, union(
                               names(combined.data[c(1:2, 4:6, 8:ncol(combined.data))]),
                               names(agg.tmp[3]))]

      # Add, the combined season code, with the name the user used in in the input
      combined.data$season__ <- s
      colnames(combined.data)[ncol(combined.data)] <- header.names[3]

      combined.data <- combined.data[,c(1:2, ncol(combined.data), 3:5, ncol(combined.data)-1,
                                        6:(ncol(combined.data)-2))]
      # -------------

      # Calculate log10 of Abundance and round it up to the next integer
      combined.data$abLog <- ceiling(log10(combined.data[, 7] + 1))

      #
      # Define Flow Score
      combined.data <- merge(combined.data, lifeFlowScore[, c(1, 3, 4)],
                             by=c(header.names[9],header.names[11]),
                             all.x=TRUE)[, union(names(combined.data),
                                                 names(lifeFlowScore[c(1, 3, 4)]))]

      # calculate Flow score per group (GroupID). Only taxa with abundance > 0!
      LIFEScores <- as.data.frame(cbind(LIFEScore=by(combined.data, combined.data$GroupID,
                      FUN=function(x) sum(x[, "fs"])/nrow(x[x[, 7] > 0, ]))))
      LIFEScores$GroupID <- rownames(LIFEScores)
      rownames(LIFEScores) <- NULL

      # merge LIFE scores to our data frame of results
      tmp.res <- merge(tmp.res, LIFEScores, by="GroupID", all.x=TRUE)[-1]

      tmp.res$Season <- s

      tmp.res <- tmp.res[c(1:2, ncol(tmp.res), 3:(ncol(tmp.res)-1))]
      colnames(tmp.res)[3] <- header.names[3]

      # Store in a dataframe all the iterations of the combined season proces.
      combined.res <- rbind(combined.res, tmp.res)

    } # end for (s in input.combined.seasons)

    # Store in a data frame the results of the whole iteration
    if (nrow(single.res) > 0) {
      results <- rbind(results, rbind(single.res, combined.res))
    } else { # there are no single season results
      results <- rbind(results, combined.res)
    }

  } else { # there are no combined seasons

    results <- rbind(results, single.res)

  } # end if (length(input.combined.seasons) > 0)

  results

} # end CalcLIFE
