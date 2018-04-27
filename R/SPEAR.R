#' @title SPEcies At Risk
#'
#' @description
#' Indicator based on biological traits used to detect effects of pesticides on
#' non-target freshwater invertebrate organisms. It can be calculated at Taxonomic
#' Levels 2, 4 and 5.
#'
#' @name CalcSPEAR
#' @aliases CalcSPEAR
#' @usage CalcSPEAR(data, season = NULL, TL = 2L,
#'           recovery.area.info = FALSE)
#'
#' @param data A dataframe containing \emph{standardised} taxa with at least
#' eight columns and in the specified order. Extra columns can be added,
#' but they must be related to Sampling Point. See 'Details'
#' @param season Optional. An integer vector containing up to 7 elements,
#' from \samp{1L} to \samp{7L}. It is only needed when the input data frame contains
#' Season and Year instead of Sample ID and Date. See 'Details' for more info about
#' input columns.
#' @param TL An integer vector containing up to 3 elements which would
#' represent the taxonomic level to calulate SPEAR at.
#' @param recovery.area.info Flag (\code{TRUE} / \code{FALSE}) to inform the
#' program whether the recovery area information has been added to the input parameters.
#' @details
#' The input dataframe \code{data} must contain a minimum of eight columns in the
#' specified order: \code{TL} (Taxonomic Level), \code{Site}, \code{Season} or \code{Sample ID},
#' \code{Year} or \code{Date} (Date object), \code{Maitland Code}, \code{Maitland Name}, \code{Abundance},
#' \code{Infered} (Code returned by the function \code{StandardiseRawTaxa}
#' which can be declared as \samp{0} if \code{StandardiseRawTaxa} is not used).
#' The presence of Recovery area Information can be included adding a ninth column
#' named \code{Rec} whose possible values are \code{NULL}, \code{1} or \code{0},
#' for \emph{unknown}, \emph{presence} and \emph{no presence}, respectively.
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
#' \code{Infered}, \code{Recovery Area information}, \code{SPEAR index},
#' \code{Water quality classes}, \code{Calculated Toxic Exposure}
#' (see 'Note'), \code{Aggregated SPEAR abundance} and \code{Aggregated SPEAR
#' Log(Abundance)}. The last two columns are merely informative and not required
#' when showing SPEAR results.
#' @note
#' More extra columns can be added to aggregate data by different criteria, i.e.,
#' other than season or Sample ID. For doing so you must fill those fields with a dummy value:
#'  \samp{1}. Thus, the aggregation would disregard season/Sample ID as an aggregation column.
#'
#' The computed \code{Calculated Toxicant Exposure} is only valid for sampling times
#' during and shortly after the main agricultural insecticide use (first 3 months of
#' the agricultural season).
#' @references
#' Liess M. & Von der Ohe P. 2005. \emph{Analyzing effects of pesticides on invertebrate
#' communities in streams}. Environ Toxicol Chem. 24: 954-965.
#'
#' Wogram J. & Liess M. 2001. \emph{Rank ordering of macroinvertebrate species sensitivity
#' to toxic compounds by comparison with that of Daphnia magna}. Bull Environ Contam
#' Toxicol. 67: 360-367
#'
#' Liess M., Schaefer R., Schriever C. 2008. \emph{The footprint of pesticide stress in
#' communities - Species traits reveal community effects of toxicants}. Science of
#' the Total Environment. 406: 484-490
#' @seealso
#' \code{\link{StandardiseRawTaxa}}
#' @examples
#' \dontrun{
#' #No examples yet
#' }
#' @export
######################################################################################
#                                                                                    #
# Version:  1.3                                                                      #
# Revision: 0 - 10/04/2014. Published version                                        #
# Revision: 1 - 17/06/2014. Added backticks to variable names used in formulae       #
#           2 - 10/02/2015. NEMS lists changed. Refactoring pending.                 #
#           3 - 30/03/2017. Fixed bug in checking aggregate method (sample/sesion).  #
#                                                                                    #
######################################################################################
CalcSPEAR <- function(data, season=NULL, TL=2L, recovery.area.info=FALSE) {

  # Input handling
  if (is.null(data))
    stop("No dataframe has been specified as 'data'")

  if (ncol(data) < 8)
    stop("It seems the input data.frame does not have the required columns")

  if (any(!TL %in% c(2L, 4L, 5L)))
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

  # Save the indices of the extra columns (and recovery area) in the data frame
  if (num.columns > 8 && recovery.area.info) {

    recovery.area.indx <- 9
    num.extracolumns <- num.columns - recovery.area.indx
    if (num.extracolumns > 0) {
      extracolumns.inds <- seq(recovery.area.indx + 1,
                               recovery.area.indx + num.extracolumns)
    } else {extracolumns.inds <- 0}

  } else if (num.columns > 8 && !recovery.area.info) {

    recovery.area.indx <- 0
    num.extracolumns <- num.columns - 8
    if (num.extracolumns > 0) {
      extracolumns.inds <- seq(9, 8 + num.extracolumns)
    } else {extracolumns.inds <- 0}

  } else { #num.columns == 8

    recovery.area.indx <- 0
    num.extracolumns <- 0
    extracolumns.inds <- 0

  }

  dfrows <- nrow(data)
  if (!recovery.area.info) data$recovery <- rep(NA, dfrows) # Recovery Area
  # and more columns: Log(1 + Abundance), and Log(SPEAR)
  data[c("abLog", "SPEARLog")] <- rep(NA, dfrows)

  # water quality classes acording to the Waterframe Directive
  # Bad:      <= 11% SPEAR
  # Poor:     > 11% and <= 22% SPEAR
  # Moderate: > 22% and <= 33% SPEAR
  # Good:     > 33% and <= 44% SPEAR
  # High:     > 44% SPEAR
  water.quality <- matrix(c(11, 22, 33, 44, 100, 1, 2, 3, 4, 5), nrow=5,
                          dimnames = list(c("Bad", "Poor", "Moderate", "Good",
                                            "High"), c("upperlimit", "value")))

  # Regression coefficients for Toxicant Exposure calculation
  # There are 3 different paramaters depending on availability of
  # "Recovery areas" information:
  # No presence of recovery areas (norec)
  # Presence of recovery areas (rec)
  # No information available (unknown)
  coeff <- list(norec=c(p1 = 1/-8.02, p2 = -1.28/-8.02),
                rec=c(p1 = 1/-6.16, p2 = -20.07/-6.16),
                unknown=c(p1 = 1/-7, p2 = -10.675/-7))

  # Combination of seasons
  combined.seasons <- list("4"=1:2, "5"=c(1,3), "6"=2:3, "7"=1:3)

  # data.frame to store the results to return by the function
  results <- data.frame()

  # TL is a number defining the TL to calculate at.
  # Note that SPEAR only calculates indices at TL2, 4 & 5.
  taxa.list.levels <- c("TL2 FAMILY", "TL5 TAXON")
  taxa.list.TL2.cols <- c("TL2 CODE", "TL2 FAMILY", "SPEAR SPECIES")
  taxa.list.TL5.cols <- c("TL5 CODE", "TL5 TAXON", "SPEAR SPECIES")
  taxa.list.cols <- as.data.frame(cbind("2"=taxa.list.TL2.cols, "5"=taxa.list.TL5.cols),
                                  stringsAsFactors=FALSE)
  spearDB <- do.call(rbind,
                     lapply(lapply(taxa.list.levels, function(i) {
                       taxa.column <- taxa.list.cols[2, substr(i, 3, 3)]
                       res <- cbind(TL=as.integer(substr(i, 3, 3)),
                                    TaxaList[TaxaList[taxa.column] != "N" &
                                                (TaxaList$`Taxon name` == TaxaList[taxa.column] |
                                                   grepl("#", TaxaList$`Taxon name`, fixed=TRUE) |
                                                   TaxaList$`Taxon name` ==
                                                   sapply(strsplit(as.character(TaxaList[taxa.column][[1]]), " "), `[`, 1)) &
                                                !is.na(TaxaList[taxa.list.cols[3, substr(i, 3, 3)]]),
                                              taxa.list.cols[, substr(i, 3, 3)]])
                       colnames(res)[2:4] <- c("CODE", "NAME", "SPEAR")
                       return(res)
                     }),
                     data.frame, row.names=NULL, stringsAsFactors=FALSE
                     )
  )

  # Retrieve SPEAR flags
  # Merge both spearDB and data to get the SPEAR flag. And replace the existing data
  # by the result.
  colnames(spearDB)[1] <- header.names[1] # Make both names equal
  colnames(spearDB)[2] <- header.names[5] # Make both names equal
  spearDB.names <- colnames(spearDB)
  # In making names used for the merging equal, we are able to maintain the
  # column order in the resulting dframe when using union in the merge.
  data <- merge(data, spearDB[c(1,2,4)], by.x=c(header.names[1],header.names[5]),
                by.y=c(spearDB.names[1],spearDB.names[2]),
                all.x=TRUE)[, union(names(data), names(spearDB[c(1,2,4)]))]

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

  # Calculate log10 of Abundance
  # RIVPACS has different criteria when dealing for TL2 and TL5 items
  # which do not score against SPEAR. Maybe it is a "bug"...
  # For TL2 the Log of abundance is calculated no matter the SPEAR score,
  # for TL5, though, the Log of abundance is 0 if SPEAR is null, that is,
  # the taxon does not have score in SPEAR database. I think the TL5 method
  # should be the correct way to go.
  data$abLog <- ifelse(data[, 1] == 2, log10(data[, 7] + 1),
                       ifelse(is.na(data$SPEAR), 0, log10(data[, 7] + 1)))

  # Define Log SPEAR for those items with SPEAR == 1
  data$SPEARLog <- ifelse(data$SPEAR == 0 | is.na(data$SPEAR), 0, data$abLog)


  if (aggregate == "season") {

    # We have calculated every season available, but we did not check whether the
    # user wanted to. We must remove those seasons which are not defined in the
    # input "season" parameter before aggregating data.
    # If there is no single season then we wil have an empty single data.frame
    single.data <- data[data[, 3] %in% season, ]

  } else { single.data <- data }

  if (nrow(single.data) > 0) {

    # Create 'single.res' data.frame (aggregated results)
    # We create it in 3 steps: unique - aggregate - merge. Because using only
    # aggregate using columns with NA values can head to incorrect results.

    # Firstly calculate number of resulting aggregated rows:
    # One per sample & season & year; Infered/Recovery info -- columns 8:9
    # and all the extra columns (if any)
    if (num.extracolumns > 0) {

      # Indices for those extra columns that only contain NAs
      NAcols <- setdiff(sapply(extracolumns.inds,
                               function(x) x * all(is.na(single.data[, x]))), 0)

      # Number of columns with NAs only
      len.NAcols <- length(NAcols)

      # Indices for those extra columns that do not contain only NAs
      cols <- setdiff(extracolumns.inds, NAcols)
      len.cols <-  length(cols)

      agg.data <- unique(single.data[, c(1:4, 8:9, 13:num.columns)])
      agg.nrows <- nrow(agg.data)

      single.res <- data.frame(agg.data[, 1:5], Rec=agg.data[, 6],
                               Sratio=rep(NA,agg.nrows),
                               Wq=rep(NA,agg.nrows), Texp=rep(NA,agg.nrows),
                               agg.data[, 7:(6 + num.extracolumns)],
                               stringsAsFactors=FALSE)

      colnames(single.res)[1:5] <- c(header.names[1:4], header.names[8])
      colnames(single.res)[10:(9 + num.extracolumns)] <- header.names[13:num.columns]

      # Aggregate 'SPEAR abundance' and 'SPEAR Log abundance'
      if (len.cols > 0) {
        factors <- sapply(c(header.names[1], header.names[2], header.names[3],
                            header.names[4], header.names[8],
                            sapply (cols, function (x) header.names[x])),
                          function (i) paste0("single.data$`",i,"`"))
      } else {
        factors <- sapply(c(header.names[1], header.names[2], header.names[3],
                            header.names[4], header.names[8]),
                          function (i) paste0("single.data$`",i,"`"))
      }


      # Create the aggregation formula
      frm <- as.formula(paste0("cbind(single.data$`", header.names[10], "`, single.data$`",
                               header.names[11], "`) ~ ", paste(factors, collapse="+")))

      agg.SPEAR.abuns <- setNames(aggregate(formula=frm, data=single.data,
                                            FUN=sum), c(names(factors), "Sabun","Slog"))

      # and merge them with 'single.res' df
      single.res <- merge(single.res, agg.SPEAR.abuns,
                          by=setdiff(intersect(names(single.res),
                                               names(agg.SPEAR.abuns)),header.names[NAcols]))

      # Order dataframe for the later rbind with combined.res
      if (len.NAcols > 0) {
        single.res <- single.res[c(1:5, (6+len.NAcols):(6+len.NAcols + 3 + len.cols),
                                   6:(5+len.NAcols),(ncol(single.res)-1):ncol(single.res))]
      } else {
        single.res <- single.res[c(1:5, (6+len.cols):(6+len.cols+3),
                                   6:(5+len.cols), (ncol(single.res)-1):ncol(single.res))]
      }

    } else {

      agg.data <- unique(single.data[, c(1:4, 8:9)])
      agg.nrows <- nrow(agg.data)

      single.res <- data.frame(agg.data[, 1:5], Rec=agg.data[, 6],
                               Sratio=rep(NA,agg.nrows),
                               Wq=rep(NA,agg.nrows), Texp=rep(NA,agg.nrows),
                               stringsAsFactors=FALSE)

      colnames(single.res)[1:5] <- c(header.names[1:4], header.names[8])

      # Aggregate 'SPEAR abundance' and 'SPEAR Log abundance'. Recovery column is
      # not needed for the aggregation.
      agg.SPEAR.abuns <- setNames(aggregate(single.data[10:11],
                                            by=c(single.data[1:4], single.data[8]),
                                            FUN=sum), c(header.names[1:4], header.names[8],
                                                        "Sabun", "Slog"))

      # and merge them with 'single.res' df
      single.res <- merge(single.res, agg.SPEAR.abuns,
                          by=intersect(names(single.res), names(agg.SPEAR.abuns)))

    }


    # Calculate SPEAR ratio
    single.res$Sratio <- ifelse(single.res$Sabun > 0,
                                100 / single.res$Sabun * single.res$Slog, 0)


    # Using upperlimit from water.quality matrix as cut points to classify Sratio
    intervals <- cut(single.res$Sratio, breaks=c(0, water.quality[, "upperlimit"]),
                     include.lowest=TRUE)

    # Create a lookup table with the factor 'intervals' and water.quality values
    # (or rownames)
    #key <- data.frame(range=levels(intervals), wq=water.quality[, "value"])
    key <- data.frame(range=levels(intervals), wq=rownames(water.quality))

    # now we can store water.quality class (we could add the character values)
    single.res$Wq <- key[match(intervals, key$range), 2]

    # Toxicant exposure, depending on availability of Recovery area information
    single.res$Texp <- ifelse(is.na(single.res$Rec), coeff$unknown[1] *
                                single.res$Sratio + coeff$unknown[2],
                              ifelse(single.res$Rec == 1, coeff$rec[1] *
                                       single.res$Sratio + coeff$rec[2],
                                     ifelse(single.res$Rec == 0, coeff$norec[1] *
                                              single.res$Sratio + coeff$norec[2], NA)))

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

      #
      # The combined seasons results are calculated by aggregating data in 2 steps
      #

      # Prepare a minimal.dataframe with common data:
      if (num.extracolumns > 0) {

        agg.combined <- unique(combined.data[, c(1, 2, 4, 5, 8:9, 12, 13:num.columns)])

      } else {

        agg.combined <- unique(combined.data[, c(1, 2, 4, 5, 8:9, 12)])

      }

      # Sum Abundance aggregating by TL, Site, Year and Code
      agg.Num_Ab <- aggregate(combined.data[, 7],
                              list(combined.data[, 1],combined.data[, 2],
                                   combined.data[, 4], combined.data[, 5],
                                   combined.data[, 8]), sum)
      colnames(agg.Num_Ab) <- c(header.names[c(1:2, 4:5, 8)], "Abun")


      # agg.combined contains only per-season abundaces for distinct families
      agg.combined <- merge(agg.combined, agg.Num_Ab)


      # Calculate log10 of Abundance. Again the TL2 - TL5 "dichotomy"...
      agg.combined$abLog <- ifelse(agg.combined[, 1] == 2,
                                   log10(agg.combined$Abun + 1),
                                   ifelse(is.na(agg.combined$SPEAR), 0,
                                          log10(agg.combined$Abun + 1)))


      # Define Log SPEAR for those items with SPEAR=1
      agg.combined$SPEARLog <- ifelse(agg.combined$SPEAR == 0 |
                                        is.na(agg.combined$SPEAR),
                                      0, agg.combined$abLog)


      # Aggregate SPEARlog and abLog by TL, site, year and 'infered'.
      # Do not need to aggregate by extra.columns since we are working with
      # combined seasons, a very specific case which only is applicable at a
      # season-level aggregation.
      agg.combined.SPEAR.abuns <- setNames(aggregate(agg.combined[(ncol(agg.combined)
                                                                   - 1):ncol(agg.combined)],
                                                     list(agg.combined[, 1], agg.combined[, 2],
                                                          agg.combined[, 3], agg.combined[, 5]),
                                                     sum),
                                           c(header.names[1], header.names[2],
                                             header.names[4], header.names[8],
                                             "Sabun", "Slog"))

      #
      # Second step/layer of aggregation
      #
      # Unique rows by TL, site, year, rec, infered (and extra columns)
      if (num.extracolumns > 0) {

        tmp.unique <- unique(agg.combined[, c(1:3, 5:6, 8:(7 + num.extracolumns))])

        # Prepare what is going to be the "result for combined seasons"
        tmp.res <- data.frame(tmp.unique[, 1:2], s, tmp.unique[, 3],
                              tmp.unique[, 4], Rec=tmp.unique[, 5],
                              tmp.unique[, 6:(5 + num.extracolumns)],
                              stringsAsFactors=FALSE)
        colnames(tmp.res)[1:5] <- c(header.names[1:4], header.names[8])
        colnames(tmp.res)[7:(6 + num.extracolumns)] <- header.names[13:num.columns]

      } else {

        tmp.unique <- unique(agg.combined[, c(1:3, 5:6)])

        # Prepare what is going to be the "result for combined seasons"
        tmp.res <- data.frame(tmp.unique[, 1:2], s, tmp.unique[, 3],
                              tmp.unique[, 4], Rec=tmp.unique[, 5],
                              stringsAsFactors=FALSE)
        colnames(tmp.res)[1:5] <- c(header.names[1:4], header.names[8])

      }



      # Combine it with Sabun and Slog results
      tmp.res <- merge(tmp.res, agg.combined.SPEAR.abuns)

      tmp.res$Sratio <- ifelse(tmp.res$Sabun > 0, 100 / tmp.res$Sabun *
                                 tmp.res$Slog, 0)

      # Using upperlimit from water.quality matrix as cut points to classify Sratio
      intervals <- cut(tmp.res$Sratio, breaks=c(0, water.quality[, "upperlimit"]),
                       include.lowest=TRUE)

      # Create a lookup table with the factor 'intervals' and water.quality values
      # (or rownames)
      #key <- data.frame(range=levels(intervals), wq=water.quality[, "value"])
      key <- data.frame(range=levels(intervals), wq=rownames(water.quality))

      # Now we can store water.quality values (we could also add the character values)
      tmp.res$Wq <- key[match(intervals, key$range), 2]

      # Toxicant exposure, depending on availability of Recovery area information
      tmp.res$Texp <- ifelse(is.na(tmp.res$Rec), coeff$unknown[1] * tmp.res$Sratio +
                               coeff$unknown[2],
                             ifelse(tmp.res$Rec == 1, coeff$rec[1] * tmp.res$Sratio +
                                      coeff$rec[2],
                                    ifelse(tmp.res$Rec == 0, coeff$norec[1] * tmp.res$Sratio +
                                             coeff$norec[2], NA)))

      #Reorder columns before rbinding
      tmp.res <- tmp.res[c(1, 2, 5, 3, 4, 6,
                           (ncol(tmp.res)-2):ncol(tmp.res),
                           7:(6 + num.extracolumns + 2))]

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

} # end CalcSPEAR
