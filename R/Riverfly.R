

# get some family TL2 data (using SEPA invertMetrics package)
# SEPAresults <- getResults(catchment = c(90,98) , startDate = '01-JAN-2015',
#                       endDate = '01-DEC-2017', analysis = 'FW_TAX_ID')

calcRiverfly <- function(data = data) {
  # need to create riverfly score for each sample number
  riverflyScore <-
    lapply(unique(data$SAMPLE_NUMBER), function(sample_number) {
      # filter results for each sample number
      SEPAresults <- data[data$SAMPLE_NUMBER == sample_number, ]

      # this table has a lookup list for riverfly taxon groups against TL2 families
      taxonTable <- read.csv(file = "inst/extdata/RIVERFLY.csv")

      # merge only works for TL2 currently - will need to merge with invert taxa table to do Tl3, Tl5 etc
      SEPAresults <-
        merge(
          SEPAresults,
          taxonTable,
          by.x = "REPORTED_NAME",
          by.y = "REPORTED_NAME",
          all.x = T
        )

      # convert to numeric abundance values
      SEPAresults$FORMATTED_ENTRY <-
        as.numeric(SEPAresults$FORMATTED_ENTRY)

      # clear any non-numeric values or '0' values just in case these have ended up in results.
      # probably a better way to deal with older data e.g. 'Present' but leave out for now.
      # removes any dodgy abundance scores - don't think there are any now in NEMS - but just in case
      SEPAresults <-
        SEPAresults[(!(
          SEPAresults$FORMATTED_ENTRY == 0 |
            is.na(SEPAresults$FORMATTED_ENTRY)
        )), ]

      # change character instead of factor
      SEPAresults$RIVERFLY_GROUP <-
        as.character(SEPAresults$RIVERFLY_GROUP)

      riverflySum <- data.frame("Cased caddis" =
                                  sum(SEPAresults$FORMATTED_ENTRY[SEPAresults$RIVERFLY_GROUP == "Cased caddis"],
                                      na.rm = T))
      riverflySum$`Olives` <-
        sum(SEPAresults$FORMATTED_ENTRY[SEPAresults$RIVERFLY_GROUP == "Olives"], na.rm = T)
      riverflySum$`Caseless caddis` <-
        sum(SEPAresults$FORMATTED_ENTRY[SEPAresults$RIVERFLY_GROUP == "Caseless caddis"],
            na.rm = T)
      riverflySum$`Flat bodied` <-
        sum(SEPAresults$FORMATTED_ENTRY[SEPAresults$RIVERFLY_GROUP == "Flat bodied"], na.rm = T)
      riverflySum$`Stoneflies` <-
        sum(SEPAresults$FORMATTED_ENTRY[SEPAresults$RIVERFLY_GROUP == "Stoneflies"], na.rm = T)
      riverflySum$`Gammarus` <-
        sum(SEPAresults$FORMATTED_ENTRY[SEPAresults$RIVERFLY_GROUP == "Gammarus"], na.rm = T)
      riverflySum$`Blue winged olives` <-
        sum(SEPAresults$FORMATTED_ENTRY[SEPAresults$RIVERFLY_GROUP == "Blue winged olives"],
                                              na.rm = T)
      riverflySum$`True Mayflies` <-
        sum(SEPAresults$FORMATTED_ENTRY[SEPAresults$RIVERFLY_GROUP == "True Mayflies"],
            na.rm = T)

      # rename column...
      riverflySum$`Cased caddis` <- riverflySum$Cased.caddis
      riverflySum$Cased.caddis  <- NULL

      # riverfly abundance categories
      category <- function(x) {
        ifelse(x > 99, 3,
               ifelse(x > 9, 2,
                      ifelse(x > 0, 1, 0)))
      }

      riverflySum[2, ] <- apply(riverflySum, 2, category)

      # replace actual abundance with riverfly categories
      riverflySum <- riverflySum[2, ]

      # sum abundancer categories to get overall riverfly score
      riverflySum$RiverflyScore <- sum(riverflySum[1, ])

      # add sample number column to allow merging back to original results
      riverflySum$SAMPLE_NUMBER <- sample_number
      return(riverflySum)

    })

  riverflyScore <- do.call("rbind", riverflyScore)
  return(riverflyScore)
}
# code test <- calcRiverfly(data = SEPAresults)
# code riverfly <- merge( SEPAresults, test, by.x="SAMPLE_NUMBER", by.y = "SAMPLE_NUMBER")
# code riverfly <- unique(riverfly[,c('SAMPLING_POINT',
#  'S_SAMPLING_PT_DESC',
#  'SAMPLED_DATE',
# 'Cased caddis',
# 'Olives',
# 'Caseless caddis',
# 'Flat bodied',
# 'Stoneflies',
# 'Gammarus',
# 'Blue winged olives',
# 'True Mayflies',
# 'RiverflyScore',
#  'S_CATCHMENT')])
# code write.csv(riverfly,file="inst/extdata/riverfly-data.csv")
