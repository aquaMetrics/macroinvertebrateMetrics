#' @title Proportion of Sediment-sensitive Invertebrates
#'
#' @description
#' A sediment-sensitive macro-invertebrate metric that provides a proxy to
#' describe the extent to which the surface of river bed are composed, or
#' covered by sediments. It can be calculated at Taxonomic Levels 3, 4 & 5.
#'
#' @name CalcPSI
#' @aliases CalcPSI
#' @usage CalcPSI(data, season = NULL, TL = 3L)
#'
#' @param data A dataframe containing \emph{standardised} taxa with at least
#' eight columns and in the specified order. Extra columns can be added,
#' but they must be related to Sampling Point. See 'Details'
#' @param season Optional. An integer vector containing up to 7 elements,
#' from \samp{1L} to \samp{7L}. It is only needed when the input data frame contains
#' Season and Year instead of Sample ID and Date. See 'Details' for more info about
#' input columns.
#' @param TL An integer vector containing up to 3 elements which would
#' represent the taxonomic level to calulate PSI at.
#' @details
#' The input dataframe \code{data} must contain a minimum of eight columns in the
#' specified order: \code{TL} (Taxonomic Level), \code{Site}, \code{Season} or \code{Sample ID},
#' \code{Year} or \code{Date} (Date object), \code{Maitland Code}, \code{Maitland Name}, \code{Abundance},
#' \code{Infered} (Code returned by the function \code{StandardiseRawTaxa}
#' which can be declared as \samp{0} if \code{StandardiseRawTaxa} is not used).
#'
#' If the data frame contains Season and Year instead of Sample ID / Date, the \code{season}
#' parameter must be a vector of the form \samp{c(1L, 3L, 5L)}, indicating which seasons, and
#' combination of seasons, must be computed.
#'
#' The available codes for \code{Season} and a general description of Taxonomic
#' Levels are defined in the main help page of the \code{aquaMetrics} package.
#' @return
#' A dataframe with the following columns: \code{TL}, \code{Site},
#' \code{Season} or \code{Sample ID}, \code{Year} or \code{Date} (Date object),
#' \code{Infered}, extra columns - if any, \code{PSIScore} and \code{R.Bed.Condition}.
#' @note
#' More extra columns can be added to aggregate data by different criteria, i.e.,
#' other than season or Sample ID. For doing so you must fill all season fields with a dummy
#' value: \samp{1}.Thus, the aggregation would disregard season/Sample ID as an aggregation column.
#'
#' Please note that this application applies some changes to standardised families
#' included in \emph{Ancylidae} and \emph{Oligochaeta} to accomodate the results
#' to those displayed in RIVPACS database (even though it may not be technically correct).
#' @references
#' Extence C., Chadd R., England J., Dunbar M., Wood P., Taylor, E. 2013.
#' \emph{The assessment of fine sediment accumulation in rivers using macro-invertebrate
#' community response}. River Res Applic. 29: 17-55.
#' @seealso
#' \code{\link{StandardiseRawTaxa}}
#' @examples
#' \dontrun{
#' #No examples yet
#' }
#' @export
######################################################################################
#                                                                                    #
# Version:  1.1                                                                      #
# Revision: 0 - 05/05/2014. Published version                                        #
#           1 - 10/02/2015. NEMS lists changed. Refactoring pending.                 #
#           2 - 30/03/2017. Fixed bug in checking aggregate method (sample/sesion).  #
#                                                                                    #
######################################################################################
CalcPSI <- function(data, season=NULL, TL=3L) {

  # Input handling
  if (is.null(data))
    stop("No dataframe has been specified as 'data'")

  if (ncol(data) < 8)
    stop("It seems the input data.frame does not have the required columns")

  if (any(!TL %in% c(3L, 4L, 5L)))
    stop("Input TL is not valid, please verify its value(s)")

  is.POSIXt <- function(x) inherits(x, "POSIXt")
  is.Date <- function(x) inherits(x, "Date")
  aggregate <- ifelse(sapply(data[4], is.POSIXt) || sapply(data[4], is.Date), "sample", "season")

  if (aggregate == "season" && any(!season %in% 1:7))
    stop("Input season is not valid, please verify its value(s)")

  header.names <- names(data)

  # Make sure we only work with user's input TL
  data <- data[data[, 1] %in% TL, ]

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


  # data.frame to store the results returned by the function
  results <- data.frame()

  # ----------------------

  # Note that PSI only calculates indices at TL3, 4 & 5.
  taxa.list.levels <- c("TL3 FAMILY", "TL5 TAXON")
  taxa.list.TL3.cols <- c("TL3 CODE", "TL3 FAMILY", "PSI GROUP")
  taxa.list.TL5.cols <- c("TL5 CODE", "TL5 TAXON", "PSI GROUP")
  taxa.list.cols <- as.data.frame(cbind("3"=taxa.list.TL3.cols, "5"=taxa.list.TL5.cols),
                                  stringsAsFactors=FALSE)
  psiDB <- do.call(rbind,
                   lapply(lapply(taxa.list.levels, function(i) {
                     taxa.column <- taxa.list.cols[2, substr(i, 3, 3)]
                    res <- cbind(TL=as.integer(substr(i, 3, 3)),
                                TaxaList[TaxaList[taxa.column] != "N" &
                                            (TaxaList$`Taxon name` == TaxaList[taxa.column] |
                                             grepl("#", TaxaList$`Taxon name`, fixed=TRUE) |
                                             TaxaList$`Taxon name` ==
                                             sapply(strsplit(as.character(TaxaList[taxa.column][[1]]), " "), `[`, 1)) &
                                            (!is.na(TaxaList[taxa.list.cols[3, substr(i, 3, 3)]]) &
                                              TaxaList[taxa.list.cols[3, substr(i, 3, 3)]] != "" ),
                                            taxa.list.cols[, substr(i, 3, 3)]])
                       colnames(res)[2:4] <- c("CODE", "NAME", "PSI")
                       return(res)
                     }),
                     data.frame, row.names=NULL, stringsAsFactors=FALSE
                     )
            )

  # Retrieve table with sensitivity ratings
  psiSensitivity <- PSISensitivity

  # Matrix. River bed condition acording to Extence et al 2013
  # Heavily sedimented:                 <= 20 PSI
  # Sedimented:                         > 21 and <= 40 PSI
  # Moderately sedimented:              > 41 and <= 60 PSI
  # Slightly sedimented:                > 61 and <= 80 PSI
  # Minimally sedimented/unsedimented:  > 81 PSI
  river.condition <- matrix(c(20, 40, 60, 80, 100, 1, 2, 3, 4, 5), nrow=5,
                            dimnames = list(c("Heavily sedimented",
                                              "Sedimented",
                                              "Moderately sedimented",
                                              "Slightly sedimented",
                                              "Minimally sedimented/unsedimented"),
                                            c("upperlimit", "value")))

  # Combination of seasons
  combined.seasons <- list("4"=1:2, "5"=c(1,3), "6"=2:3, "7"=1:3)


  # Fill an ID column which would group items by TL, site, year, season,
  # infered and extra-columns
  # Hopefully, it will make easier further aggregations and calcs.
  if (num.extracolumns > 0) {
    data$GroupID <- as.numeric(factor(do.call(paste, data[, c(1:4, 8, extracolumns.inds)])))
  } else {
    data$GroupID <- as.numeric(factor(do.call(paste, data[, c(1:4, 8)])))
  }

  # ---------------------
  # PSI hack to obtain results comparable to RIVPACS
  # TL3:
  #  - Change all oligochaetes worms 20xxxxxx to their 20000000 Oligachaeta subclass
  #  - Include Ancylidae into Planorbidae
  if (3L %in% TL) {
    data[data[, 1]==3 & substr(data[, 5], 1, 2)=="20", c(5:6)] <-
      list("20000000", "Oligochaeta")
    data[data[, 1]==3 & substr(data[, 5], 1, 4)=="1624", c(5:6)] <-
      list("16230000", "Planorbidae")

    # Tip: when merging, order of factors: from quickest varying factors to slowest
    agg.tmp <- setNames(aggregate(data[, 7] ~  data[, 5] + data$GroupID,
                                  data=data, FUN=sum),
                        c(header.names[5], "GroupID", header.names[7]))

    # Merge and reorder to obtain a new dataframe similar to the original but
    # having the the new groups aggregated
    data <- merge(unique(data[c(1:6,8:ncol(data))]), agg.tmp,
                  by=c(header.names[5], "GroupID"))[, union(names(data[c(1:6, 8:ncol(data))]),
                                      names(agg.tmp[3]))]
    data <- data[,c(1:6, ncol(data), 7:(ncol(data)-1))]

  }
  # ---------------------

  # Retrieve PSI groups
  # Merge both psiDB and data to get the PSI group. And replace the existing data
  # by the result.
  colnames(psiDB)[1] <- header.names[1] # Make both names equal
  colnames(psiDB)[2] <- header.names[5] # Make both names equal
  psiDB.names <- colnames(psiDB)
  # In making names used for the merging equal, we are able to maintain the
  # column order in the resulting dframe when using union in the merge.
  data <- merge(data, psiDB[c(1, 2, 4)], by=c(header.names[1], header.names[5]),
                all.x=TRUE)[, union(names(data), names(psiDB[c(1, 2, 4)]))]

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

  # Define Sensitivity Score
  colnames(psiSensitivity)[1] <- header.names[11] # Make both names equal
  colnames(psiSensitivity)[3] <- header.names[9] # Make both names equal
  psiSensitivity.names <- colnames(psiSensitivity)
  data <- merge(data, psiSensitivity[, c(1, 3, 4)],
                       by=c(header.names[9],header.names[11]),
                       all.x=TRUE)[, union(names(data),
                                           names(psiSensitivity[c(1, 3, 4)]))]
  num.columns <- ncol(data)

  if (aggregate == "season") {

    # We have calculated every season available, but we did not check whether the
    # user wanted to. We must remove those seasons which are not defined in the
    # input "season" parameter, before aggregating data.
    # If there is no single season then we wil have an empty single data.frame
    single.data <- data[data[, 3] %in% season, ]

  } else { single.data <- data }

  # And we should also remove those elments whose PSI is NA or "E"
  single.data <- single.data[!is.na(single.data$PSI) & single.data$PSI != "E", ]

  if (nrow(single.data) > 0) {

    if (num.extracolumns > 0) {

      # Create 'single.res' data.frame (aggregated results)
      single.res <- unique(single.data[, c(1:4, 8, 10, extracolumns.inds)])

    } else {

      # Create 'single.res' data.frame (aggregated results)
      single.res <- unique(single.data[, c(1:4, 8, 10)])

    }

    # calculate PSI score per group (GroupID)
    PSIScores <- as.data.frame(cbind(PSIScore=by(single.data, single.data$GroupID,
      FUN=function(x)
        sum(x[x$PSI=="A" | x$PSI=="B", "SEDIMENT.SS"], na.rm = TRUE) /
          sum(x$SEDIMENT.SS, na.rm = TRUE) * 100)))
    PSIScores$GroupID <- rownames(PSIScores)
    rownames(PSIScores) <- NULL

    # merge PSI scores to our data frame of results
    single.res <- merge(single.res, PSIScores, by="GroupID", all.x=TRUE)[-1]


    # Using upperlimit from river.condition matrix as cut points to classify PSIScore
    intervals <- cut(single.res$PSIScore, breaks=c(0, river.condition[, "upperlimit"]),
                     include.lowest=TRUE)

    # Create a lookup table with the factor 'intervals' and river.condition values
    # (or rownames)
    #key <- data.frame(range=levels(intervals), rc=river.condition[, "value"])
    key <- data.frame(range=levels(intervals), rc=rownames(river.condition))

    # now we can store river.condition class
    single.res$R.Bed.Condition <- key[match(intervals, key$range), 2]


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
      # Remove Sensitivity
      combined.data[, names(combined.data)[ncol(combined.data)]] <- NULL
      # Reset abLog
      combined.data$abLog <- NA
      # Remove those elments whose PSI is NA or "E"
      combined.data <- combined.data[!is.na(combined.data$PSI) &
                                       combined.data$PSI != "E", ]

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
      # Aggregate and merge, analogous to what it was done in "PSI hack" above
      # but leaving out "season" for the aggregation
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
      # Define Sensitivity Score
      combined.data <- merge(combined.data, psiSensitivity[, c(1, 3, 4)],
                    by=c(header.names[9],header.names[11]),
                    all.x=TRUE)[, union(names(combined.data),
                                        names(psiSensitivity[c(1, 3, 4)]))]

      # calculate PSI score per group (GroupID)
      PSIScores <- as.data.frame(cbind(PSIScore=by(combined.data, combined.data$GroupID,
        FUN=function(x)
        sum(x[x$PSI=="A" | x$PSI=="B", "SEDIMENT.SS"], na.rm = TRUE) /
          sum(x$SEDIMENT.SS, na.rm = TRUE) * 100)))
      PSIScores$GroupID <- rownames(PSIScores)
      rownames(PSIScores) <- NULL

      # merge PSI scores to our data frame of results
      tmp.res <- merge(tmp.res, PSIScores, by="GroupID", all.x=TRUE)[-1]


      # Using upperlimit from river.condition matrix as cut points to classify PSIScore
      intervals <- cut(tmp.res$PSIScore, breaks=c(0, river.condition[, "upperlimit"]),
                       include.lowest=TRUE)

      # Create a lookup table with the factor 'intervals' and river.condition values
      # (or rownames)
      #key <- data.frame(range=levels(intervals), rc=river.condition[, "value"])
      key <- data.frame(range=levels(intervals), rc=rownames(river.condition))

      # now we can store river.condition class
      tmp.res$R.Bed.Condition <- key[match(intervals, key$range), 2]
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

} # end CalcPSI
