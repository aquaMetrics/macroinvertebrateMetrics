awic <- function(data,
                      names = macroinvertebrateMetrics::column_attributes$name,
                      questions = c(
                        "Taxon abundance",
                        "Taxon Abundance"
                      ),
                      metric_cols = metric_cols) {
  metric_cols <- metric_cols[metric_cols$metric == "awic", ]
  data <- select(data, any_of(c(names,  metric_cols$metric_names)))

  # Replace value in awic_score column with matching values in AWIC_A, AWIC_B,
  # AWIC_C based on 'RESULT' in this case value is Abundance.
  results <- dplyr::rename(data,
    "response" = !!column_attributes$name[3]
  )
  results <-
    dplyr::mutate(results,
      awic_score = dplyr::if_else(.data$response > 99, .data$AWIC_C,
        dplyr::if_else(.data$response > 9,
          .data$AWIC_B,
          .data$AWIC_A
        )
      )
    )

  results <- stats::na.omit(results)
  # Group by sample_id to calculate score per sample
  results <- dplyr::rename(results,
    "sample_id" = !!column_attributes$name[1]
  )
  results <- dplyr::group_by(results, .data$sample_id)

  # `summarise` calculates `sum()` and `n()` on what we grouped by(sample_number)
  scores <- dplyr::summarise(results,
    sample_score = sum(.data$awic_score),
    ntaxa = n()
  )
  # Create (or 'mutate') a new column called wfd_awic (sample_score / ntaxa)
  scores <- scores %>%
    dplyr::mutate(wfd_awic = .data$sample_score / .data$ntaxa)

  scores <- pivot_longer(scores,
    cols = c("sample_score", "ntaxa", "wfd_awic"),
    names_to = column_attributes$name[2],
    values_to = column_attributes$name[3]
  )

  scores <- dplyr::mutate(
    scores,
    !!column_attributes$name[5] := "WFD_AWIC",
    !!column_attributes$name[6] := "WFD AWIC"
  )

  scores <- dplyr::mutate_at(scores,
                           column_attributes$name[3], as.character)
  return(scores)
}
