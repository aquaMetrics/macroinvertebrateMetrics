#' @title Riverfly Angler's Score Index
#'
#' @description
#' The Riverfly Partnership monitoring initiative aims to address perceived
#' declines in the abundance of target benthic macroinvertebrates (mostly river
#' flies).
#'
#' @name CalcASI
#' @aliases CalcASI
#' @usage CalcASI(data, season = NULL, TL = 3L)
#' @param data A dataframe containing \emph{standardised} taxa with at least
#' eight columns and in the specified order. Extra columns can be added,
#' but they must be related to Sampling Point. See 'Details'
#' @param season Optional. An integer vector containing up to 7 elements,
#' from \samp{1L} to \samp{7L}. It is only needed when the input data frame contains
#' Season and Year instead of Sample ID and Date. See 'Details' for more info about
#' input columns.
#' @param TL An integer vector containing code \samp{3}. Which is the closest TL
#' to include the Angler's Score Index in. \samp{3} is the default value anyway,
#' because this index actually is not defined in any specific TL, so it can be ignored.
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
#' Although there is no output TL defined for this index, a default value of \samp{3}
#' has been chosen to keep the same signature as similar functions in the package.
#'
#' The available codes for \code{Season} and a general description of Taxonomic
#' Levels are defined in the main help page of the \code{aquaMetrics} package.
#' @return
#'  A dataframe with the following columns: \code{TL}, \code{Site},
#' \code{Season} or \code{Sample ID}, \code{Year} or \code{Date} (Date object),
#' \code{Infered}, extra columns - if any, and \code{ASI} (Angler's Score Index).
#' @note
#' The extra columns can be used to aggregate data by different criteria, i.e.,
#' other than season or Sample ID. For doing so you must fill all season fields with a dummy
#' value \samp{1}. Thus, the aggregation will disregard season/Sample ID as an aggregation column.
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
# Revision: 0 - 05/06/2014. Published version                                        #
#           1 - 10/02/2015. NEMS lists changed. Refactoring pending.                 #
#           2 - 30/03/2017. Fixed bug in checking aggregate method (sample/sesion).  #
#                                                                                    #
######################################################################################
CalcASI <- function(data, season=NULL, TL=3L) {

  # Input handling
  if (is.null(data))
    stop("No dataframe has been specified as 'data'")

  if (ncol(data) < 8)
    stop("It seems the input data.frame does not have the required columns")

  if (TL != 3L)
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
  # Group ID --to group rows by TL, season, year, site, and extra columns
  data["GroupID"] <- rep(NA, dfrows)


  # data.frame to store the results to return by the function
  results <- data.frame()

  #  ----------------------

  # Note that ASI only calculates indices at (kind of) "TL3".
  asiDB <- LookUpASI

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

  # Retrieve ASI groups
  # Merge both asiDB and data to get the ASI group. And replace the existing data
  # by the result.
  colnames(asiDB)[1] <- header.names[1] # Make both names equal
  colnames(asiDB)[2] <- header.names[5] # Make both names equal
  asiDB.names <- colnames(asiDB)
  # In making names used for the merging equal, we are able to maintain the
  # column order in the resulting dframe when using union in the merge.
  data <- merge(data, asiDB[c(1, 2, 4)], by=c(header.names[1],header.names[5]),
                all.x=TRUE)[, union(names(data), names(asiDB[c(1, 2, 4)]))]

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

    # We have worked with every season available, but we did not check whether the
    # user wanted to. We must remove those seasons which are not defined in the
    # input "season" parameter, before aggregating data.
    # If there is no single season then we wil have an empty single data.frame
    single.data <- data[data[, 3] %in% season, ]

  } else { single.data <- data }

  # And we should also remove those elments whose ASI is NA
  single.data <- single.data[!is.na(single.data$ASI), ]

  if (nrow(single.data) > 0) {

    if (num.extracolumns > 0) {

      # Create 'single.res' data.frame (aggregated results)
      single.res <- unique(single.data[, c(1:4, 8:9, extracolumns.inds)])

    } else {

      # Create 'single.res' data.frame (aggregated results)
      single.res <- unique(single.data[, c(1:4, 8:9)])

    }

    # calculate ASI score per group (GroupID)
    ASIScores <- sapply(split(single.data, single.data$GroupID), function(group){
      # sum abundance per ASI target groups (there are 8 of them), then calculate
      # their logarithm which in turn becomes the score used to get the final ASI score.
      s1 <- ceiling(log10(tapply(group[, 7], group$ASI, sum) + 1))
      # scores for 1000+ organism are always 4 (no matter which is the value
      # of the logarithm)
      s <- sum(ifelse(s1 > 4, 4, s1))
    })

    ASIScores <- data.frame(GroupID=names(ASIScores), ASI=ASIScores)

    # merge ASI scores to our data frame of results
    single.res <- merge(single.res, ASIScores, by="GroupID", all.x=TRUE)[-1]


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


      # Remove those elments whose ASI is NA
      combined.data <- combined.data[!is.na(combined.data$ASI), ]

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


      # calculate ASI score per group (GroupID)
      ASIScores <- sapply(split(combined.data, combined.data$GroupID), function(group){
        # sum abundance per ASI target groups (there are 8 of them), then calculate
        # their logarithm which in turn becomes the score used to get the final ASI score.
        s1 <- ceiling(log10(tapply(group[, 7], group$ASI, sum) + 1))
        # scores for 1000+ organisms are always 4 (no matter which is the value
        # of the logarithm)
        s <- sum(ifelse(s1 > 4, 4, s1))
      })

      ASIScores <- data.frame(GroupID=names(ASIScores), ASI=ASIScores)

      # merge ASI scores to our data frame of results
      tmp.res <- merge(tmp.res, ASIScores, by="GroupID", all.x=TRUE)[-1]

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

} # end CalcASI
