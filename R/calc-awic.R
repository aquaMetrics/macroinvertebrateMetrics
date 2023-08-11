#' Acid WFD-AWIC metric
#'
#' @param data dataframe containing mixtaxon invertebrates
#' @param names
#' Optional, user provided list of column names different to those used
#' in `column_attributes` to match with input data
#' @param questions
#' Optional, user provided 'question' default is 'Taxon abundance', which
#' filters only abundance values.
#' @return dataframe
#' @export
#' @importFrom dplyr n select mutate
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data :=
#' @examples
#' metricResults <- calc_awic(demo_data)
calc_awic <- function(data,
                      names = column_attributes$name,
                      questions = c("Taxon abundance",
                                    "Taxon Abundance")) {
  browser()
  # To allow user to specify the names of the columns to match the columns in
  # their dataset update package column name data with column names provided to
  # function
  column_attributes$name <- names
  # Validate and format input data - column names are user provided or revert to
  # default. After this point, columns names are referred by index/number rather
  # than text of column name, this allows the default column names to be update
  # easily in future
  data <- validate_input(data, names = names)
  data <- select (data, !!column_attributes$name[1],
                  !!column_attributes$name[4],
                  !!column_attributes$name[3])

  # Filter macroinvertebrate scores for only AWIC scores
  # This helps keep the data comprehensible and only things we are interested in
  taxa <- macroinvertebrateMetrics::macroinvertebrateTaxa
  taxa <- taxa[, c("TAXON_NAME", "AWIC_A", "AWIC_B", "AWIC_C")]

  # Merge results and scores
  results <-
    merge(data,
          taxa,
          by.x = column_attributes$name[4],
          by.y = "TAXON_NAME"
    )
  if (nrow(results) < 1) {
    return(NULL)
  }

  results <- dplyr::filter(results, !is.na(.data$AWIC_A))
  # Replace value in awic_score column with matching values in AWIC_A, AWIC_B,
  # AWIC_C based on 'RESULT' in this case value is Abundance.
  results <- dplyr::rename(results,
                    "response" = !!column_attributes$name[3])
  results <-
    dplyr::mutate(results,
                  awic_score = dplyr::if_else(.data$response > 99, .data$AWIC_C,
                          dplyr::if_else(.data$response > 9,
                                         .data$AWIC_B,
                                         .data$AWIC_A)
                                         )
                  )


#   results$awic_score[results$RESULT > 0] <- results$AWIC_A[results$RESULT > 0]
#   results$awic_score[results$RESULT > 9] <- results$AWIC_B[results$RESULT > 9]
#   results$awic_score[results$RESULT > 99] <- results$AWIC_C[results$RESULT > 99]

  results <- stats::na.omit(results)

  # Group by SAMPLE_ID to calculate score per sample
  results <- dplyr::rename(results,
                    "sample_id" = !!column_attributes$name[1])
  results <- dplyr::group_by(results, .data$sample_id)

  # `summarise` calculates `sum()` and `n()` on what we grouped by(sample_number)
  scores <- dplyr::summarise(results,
    sample_score = sum(.data$awic_score),
    ntaxa = n()
  )
  # Create (or 'mutate') a new column called wfd_awic (sample_score / ntaxa)
  scores <- scores %>%
    mutate(wfd_awic = .data$sample_score / .data$ntaxa)

  scores <- pivot_longer(scores,
    cols = c("sample_score", "ntaxa", "wfd_awic"),
    names_to = column_attributes$name[2],
    values_to = column_attributes$name[3]
  )

  scores <- mutate(scores,
    !!column_attributes$name[5] := "WFD_AWIC",
    !!column_attributes$name[6] := "WFD AWIC"
  )

  return(scores)
}
