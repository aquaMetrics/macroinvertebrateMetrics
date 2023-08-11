#' Riverfly metric
#'
#' Function to calculate riverfly scores
#'
#' @param data
#' A data frame of ecology data as defined in `column_attributes`.
#' @param names
#' Optional, user provided list of column names different to those used
#' in `column_attributes` to match with input data
#' @param questions
#' Optional, user provided 'question' default is 'Taxon abundance', which
#' filters only abundance values.
#' @return
#' A data frame 5 variables
#' @export
#' @importFrom rlang .data :=
#' @importFrom magrittr "%>%"
#' @importFrom dplyr all_of across
#' @examples
#' data <- demo_data
#' data <- data[data$parameter == "River Family Inverts", ]
#' output <- calc_riverfly(data)
#'
calc_riverfly <- function(data,
                          names = macroinvertebrateMetrics::column_attributes$name,
                          questions = c("Taxon abundance",
                                        "Taxon Abundance",
                                        "Live abundance")
                          ){
  # To allow user to specify the names of the columns to match the columns in
  # their dataset update package column name data with column names provided to
  # function
  column_attributes <- macroinvertebrateMetrics::column_attributes
  column_attributes$name <- names
  # Validate and format input data - column names are user provided or revert to
  # default. After this point, columns names are referred by index/number rather
  # than text of column name, this allows the default column names to be update
  # easily in future
  data <- validate_input(data, names = names)

  macroinvertebrates <- macroinvertebrateMetrics::macroinvertebrateTaxa

  sepa_results <-
    merge(data,
      macroinvertebrates,
      by.x = column_attributes$name[4],
      by.y = "TAXON_NAME"
    )

  # This table has a lookup list for riverfly taxon groups against TL2 families
  taxon_table <- utils::read.csv(
    system.file("extdat",
      "riverfly.csv",
      package = "macroinvertebrateMetrics"
    ),
    stringsAsFactors = FALSE
  )

  # Merge only works for TL2 currently - will need to merge with invert taxa
  # table to do Tl3, Tl5 etc
  riverfly_taxa <-
    merge(
      sepa_results[, c(
        "TL2_5_TAXON",
        column_attributes$name[4],
        column_attributes$name[3],
        column_attributes$name[1]
      )],
      taxon_table,
      by.x = "TL2_5_TAXON",
      by.y = "REPORTED_NAME"
    )

  # Need to create riverfly score for each sample number
  riverfly_sum <- dplyr::group_by(
    riverfly_taxa,
    across(all_of(c(
      "RIVERFLY_GROUP",
      column_attributes$name[1]
    )))
    # .data$SAMPLE_ID
  ) %>%
    dplyr::summarise(across(column_attributes$name[3], sum))

  # riverfly abundance categories
  category <- function(x) {
    ifelse(x > 99, 3,
      ifelse(x > 9, 2,
        ifelse(x > 0, 1, 0)
      )
    )
  }
  riverfly_sum$VALUE_LOG <- c(apply(
    riverfly_sum[, column_attributes$name[3]], 2, category
  ))

  # group_by sample_id and sum log abundance
  riverfly_score <- dplyr::group_by(
    riverfly_sum,
    across(column_attributes$name[1])
  ) %>%
    dplyr::summarise(response = sum(.data$VALUE_LOG))
  # if no relevant data return NULL object
  if (nrow(riverfly_score) == 0) {
    return()
  }

  riverfly_score <- mutate(
    riverfly_score,
    !!column_attributes$name[2] := "Riverfly Score",
    !!column_attributes$name[5] := "Riverfly Metric",
    !!column_attributes$name[6] := "Angler Riverfly Monitoring Index (ARMI)"
  )

  riverfly_score <- select(
    riverfly_score,
    column_attributes$name[1],
    column_attributes$name[2],
    column_attributes$name[3],
    column_attributes$name[5],
    column_attributes$name[6]
  )
  return(riverfly_score)
}
