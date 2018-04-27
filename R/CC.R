#' @title Climate Change Metric
#'
#' @description
#' A temperature metric that computes the ratio or porportion of organisms with
#' different temperature tolerances. It can be calculated at Taxonomic Levels 5 and 4
#'
#' @name CalcCC
#' @aliases CalcCC
#' @usage CalcCC(data, season = NULL, TL = 5L,
#'        method = 1L)
#' @param data A dataframe containing \emph{standardised} taxa with at least
#' eight columns and in the specified order. Extra columns can be added,
#' but they must be related to Sampling Point. See 'Details'
#' @param season Optional. An integer vector containing up to 7 elements,
#' from \samp{1L} to \samp{7L}. It is only needed when the input data frame contains
#' Season and Year instead of Sample ID and Date. See 'Details' for more info about
#' input columns.
#' @param TL An integer vector containing up to 2 elements which would
#' represent the taxonomic level to calulate Climate Change at.
#' @param method It can take either take the numeric value \samp{1L}, which
#' computes the percentage of indicators, or \samp{2} which calculates the
#' proportion of \emph{n} thermal indicators (useful when data contain log
#' abundances rather than counts).
#' @details
#' The input dataframe \code{data} must contain a minimum of eight columns in the
#' specified order: \code{TL} (Taxonomic Level), \code{Site}, \code{Season} or \code{Sample ID},
#' \code{Year} or \code{Date} (Date object), \code{Maitland Code}, \code{Maitland Name}, \code{Abundance},
#' \code{Infered} (Code returned by the function \code{StandardiseRawTaxa}
#' which can be declared as \samp{0} if \code{StandardiseRawTaxa} is not used).

#' If the data frame contains Season and Year instead of Sample ID / Date, the \code{season}
#' parameter must be a vector of the form \samp{c(1L, 3L, 5L)}, indicating which seasons, and
#' combination of seasons, must be computed.

#' This function works with both current Maitland Codes and new ones,
#' defined as of JUL-2014.
#'
#' The available codes for \code{Season} and a general description of Taxonomic
#' Levels are defined in the main help page of the \code{aquaMetrics} package.
#' @return
#'  A dataframe with the following columns: \code{TL}, \code{Site},
#' \code{Season} or \code{Sample ID}, \code{Year} or \code{Date} (Date object),
#' \code{Infered}, extra columns - if any, and \emph{CC metrics} which contain
#' \code{CS} (Cold Stenotherms), \code{WS} (Warm Stenotherms), \code{E} (Eurytherms) and
#' \code{TIndicators} (Percentage or proportion of thermal indicators in the
#' sample).
#' @note
#' The extra columns can be used to aggregate data by different criteria, i.e.,
#' other than season or Sample ID. For doing so you must fill all season fields with a dummy
#' value \samp{1}. Thus, the aggregation will disregard season/SampleID as an aggregation column.
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
# Revision: 0 - 26/08/2014. Published version                                        #
#           1 - 10/02/2015. NEMS lists changed. Refactoring pending.                 #
#           2 - 30/03/2017. Fixed bug in checking aggregate method (sample/sesion).  #
#                                                                                    #
######################################################################################
CalcCC <- function(data, season=NULL, TL=5L, method=1L) {

  # Input handling
  if (is.null(data))
    stop("No dataframe has been specified as 'data'")

  if (ncol(data) < 8)
    stop("It seems the input data.frame does not have the required columns")

  if (any(!TL %in% c(4L, 5L)))
    stop("Input TL is not valid, please verify its value(s)")

  is.POSIXt <- function(x) inherits(x, "POSIXt")
  is.Date <- function(x) inherits(x, "Date")
  aggregate <- ifelse(sapply(data[4], is.POSIXt) || sapply(data[4], is.Date), "sample", "season")

  if (aggregate == "season" && any(!season %in% 1:7))
    stop("Input season is not valid, please verify its value(s)")

  if (any(!method %in% c(1L, 2L)))
    stop("Metric calculation method is not valid, please verify its value(s)")

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
  # Group ID --to group rows by TL, season, year, site, and extra columns
  data["GroupID"] <- rep(NA, dfrows)


  # data.frame to store the results to return by the function
  results <- data.frame()

  #  ----------------------

  # Note that CC calculates indices at TL5.
  ccDB <- LookUpCC

  # Combination of seasons
  combined.seasons <- list("4"=1:2, "5"=c(1,3), "6"=2:3, "7"=1:3)

  # Remove data with 0 abundance. We do not need it
  data <- data[data[, 7] > 0, ]

  # Fill an ID column which would group items by TL, site, year, season,
  # infered and extra-columns
  # Hopefully, it will make easier further aggregations and calcs.
  if (num.extracolumns > 0) {
    data$GroupID <- as.numeric(factor(do.call(paste, data[, c(1:4, 8, extracolumns.inds)])))
  } else {
    data$GroupID <- as.numeric(factor(do.call(paste, data[, c(1:4, 8)])))
  }

  # ---------------------

  # Retrieve CC temperature ref.
  # Merge both ccDB and data to get the CC temp. And replace the existing data
  # by the result.
  colnames(ccDB)[1] <- header.names[1] # Make both names equal
  colnames(ccDB)[2] <- header.names[5] # Make both names equal
  ccDB.names <- colnames(ccDB)
  # In making names used for the merging equal, we are able to maintain the
  # column order in the resulting dframe when using union in the merge.
  data <- merge(data, ccDB[c(1, 2, 4, 5)], by=c(header.names[1],header.names[5]),
                all.x=TRUE)[, union(names(data), names(ccDB[c(1, 2, 4, 5)]))]

  # Convert those NA in column TEMP to text so that we don't have to check
  # for NA's all the time)
  data[is.na(data$TEMP), "TEMP"] <- "N/A"

  # The CC list contains the extra column NEW.CODE - This list, contains some
  # taxa twice, encoded with both their current Maitland code and the new one.
  # Here we will swap all those codes for NEW.CODE, to make the list more
  # consistent.
  data[!is.na(data$NEW.CODE), 5] <- data[!is.na(data$NEW.CODE), "NEW.CODE"]


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

  if (aggregate == "season") {

    # We  haven't check yet whether the user want to calculate all seasons.
    # We must remove those seasons which are not defined in the input "season"
    # parameter, before aggregating data. If there is no single season then we
    # will have an empty single data.frame
    single.data <- data[data[, 3] %in% season, ]

  } else { single.data <- data }

  if (nrow(single.data) > 0) {

    if (num.extracolumns > 0) {

      # Create 'single.res' data.frame (aggregated results)
      single.res <- unique(single.data[, c(1:4, 8:9, extracolumns.inds)])

    } else {

      # Create 'single.res' data.frame (aggregated results)
      single.res <- unique(single.data[, c(1:4, 8:9)])

    }

    # calculate CC metrics per group (GroupID)
    if (method == 1L) { # %age of total Indicator Taxa abundance

      CCMetrics <- as.data.frame(cbind(
        CS=by(single.data, single.data$GroupID,
              FUN=function(x)
                sum(x[x$TEMP=="CS", 7]) /
                sum(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E" , 7]) * 100),
        WS=by(single.data, single.data$GroupID,
              FUN=function(x)
                sum(x[x$TEMP=="WS", 7]) /
                sum(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E" , 7]) * 100),
        E=by(single.data, single.data$GroupID,
              FUN=function(x)
                sum(x[x$TEMP=="E", 7]) /
                sum(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E" , 7]) * 100),
        TIndicators=by(single.data, single.data$GroupID,
              FUN=function(x)
                sum(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E", 7]) /
                sum(x[, 7]) * 100)))
      CCMetrics$GroupID <- rownames(CCMetrics)
      rownames(CCMetrics) <- NULL

    } else if (method == 2L) { # proportion of n thermal indicator taxa

      CCMetrics <- as.data.frame(cbind(
        CS=by(single.data, single.data$GroupID,
              FUN=function(x)
                length(x[x$TEMP=="CS", 7]) /
                length(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E" , 7]) * 100),
        WS=by(single.data, single.data$GroupID,
              FUN=function(x)
                length(x[x$TEMP=="WS", 7]) /
                length(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E" , 7]) * 100),
        E=by(single.data, single.data$GroupID,
             FUN=function(x)
               length(x[x$TEMP=="E", 7]) /
               length(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E" , 7]) * 100),
        TIndicators=by(single.data, single.data$GroupID,
              FUN=function(x)
                length(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E", 7]) /
                length(x[, 7]) * 100)))
      CCMetrics$GroupID <- rownames(CCMetrics)
      rownames(CCMetrics) <- NULL

    }

    # merge CC metrics to our data frame of results
    single.res <- merge(single.res, CCMetrics, by="GroupID", all.x=TRUE)[-1]

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

      # Prepare a minimal.dataframe with common data:
      if (num.extracolumns > 0) {

        # Create a new GroupID as we do not have season here
        combined.data$GroupID <- as.numeric(factor(do.call(
          paste, combined.data[, c(1:2, 4, 8, extracolumns.inds)])))

        tmp.res <- unique(combined.data[, c(1:2, 4, 8:9, extracolumns.inds)])

      } else {

        combined.data$GroupID <- as.numeric(factor(do.call(
          paste, combined.data[, c(1:2, 4, 8)])))

        tmp.res <- unique(combined.data[, c(1:2, 4, 8:9)])

      }

      # -------------
      # Aggregate and merge abundances by Mailand codes and GroupID
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

      combined.data <- combined.data[, c(1:2, ncol(combined.data), 3:5, ncol(combined.data)-1,
                                        6:(ncol(combined.data)-2))]
      # -------------

      # calculate CC metrics per group (GroupID)
      if (method == 1L) { # %age of total Indicator Taxa abundance

        CCMetrics <- as.data.frame(cbind(
          CS=by(combined.data, combined.data$GroupID,
                FUN=function(x)
                  sum(x[x$TEMP=="CS", 7]) /
                  sum(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E" , 7]) * 100),
          WS=by(combined.data, combined.data$GroupID,
                FUN=function(x)
                  sum(x[x$TEMP=="WS", 7]) /
                  sum(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E" , 7]) * 100),
          E=by(combined.data, combined.data$GroupID,
               FUN=function(x)
                 sum(x[x$TEMP=="E", 7]) /
                 sum(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E" , 7]) * 100),
          TIndicators=by(combined.data, combined.data$GroupID,
               FUN=function(x)
                 sum(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E", 7]) /
                 sum(x[, 7]) * 100)))
        CCMetrics$GroupID <- rownames(CCMetrics)
        rownames(CCMetrics) <- NULL

      } else { # proportion of n thermal indicator taxa

        CCMetrics <- as.data.frame(cbind(
          CS=by(combined.data, combined.data$GroupID,
                FUN=function(x)
                  length(x[x$TEMP=="CS", 7]) /
                  length(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E" , 7]) * 100),
          WS=by(combined.data, combined.data$GroupID,
                FUN=function(x)
                  length(x[x$TEMP=="WS", 7]) /
                  length(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E" , 7]) * 100),
          E=by(combined.data, combined.data$GroupID,
               FUN=function(x)
                 length(x[x$TEMP=="E", 7]) /
                 length(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E" , 7]) * 100),
          TIndicators=by(combined.data, combined.data$GroupID,
               FUN=function(x)
                 length(x[x$TEMP=="CS" | x$TEMP=="WS" | x$TEMP=="E", 7]) /
                 length(x[, 7]) * 100)))
        CCMetrics$GroupID <- rownames(CCMetrics)
        rownames(CCMetrics) <- NULL

      }

      # merge CC metrics to our data frame of results
      tmp.res <- merge(tmp.res, CCMetrics, by="GroupID", all.x=TRUE)[-1]

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

} # end CalcCC
