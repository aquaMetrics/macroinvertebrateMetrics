validate_input <- function(
    data,
    names,
    questions,
    taxon_table = macroinvertebrateMetrics::macroinvertebrateTaxa,
    metric_cols = macroinvertebrateMetrics::metric_cols) {
  column_attributes <- macroinvertebrateMetrics::column_attributes
  column_attributes$name <- names
  stopifnot(ncol(data[, names(data) %in% column_attributes$name[c(1, 3:4)]]) == 3)
  data <- dplyr::select(
    data,
    dplyr::any_of(column_attributes$name)
  )

  # If 'question' column provided, select only 'Taxon abundance' required for most metrics...
  if (any(names(data) %in% column_attributes$name[2])) {
    data <- data[unlist(data[, column_attributes$name[2]]) %in% questions, ]
  }
  ## as.numeric function that preserves numeric values when converting factor to
  ## numeric
  as_numeric_mod <- function(x) {
    if (is.factor(x)) {
      as.numeric(levels(x))[x]
    } else {
      as.numeric(x)
    }
  }

  ## convert column types to required type
  convert_magic <- function(obj, types) {
    out <- lapply(
      1:length(obj),
      FUN = function(i) {
        FUN1 <- switch(types[i],
          character = as.character,
          numeric = as_numeric_mod,
          factor = as.factor
        )
        FUN1(unlist(obj[, i]))
      }
    )
    names(out) <- colnames(obj)
    as.data.frame(out, stringsAsFactors = FALSE)
  }


  # only convert columns present
  convert_columns <- dplyr::filter(
    column_attributes,
    column_attributes$name %in% names(data)
  )
  data <- suppressWarnings(convert_magic(data, convert_columns$col_type))

  # Tidy TAXON name incase of whitespace
  data[, column_attributes$name[4]] <-
    trimws(data[, column_attributes$name[4]])

  # Filter results so only Taxon abundance greater zero
  # (sometimes errors and zero or less are accidentally recorded)
  data <- dplyr::filter(data, .data$response > 0)
  # Select metric score columns and taxon name from taxon table
  taxon_table <- taxon_table[, c("TAXON_NAME", unique(metric_cols$metric_names))]

  # Need to join data to reference table of metric scores
  data <-
    merge(
      data,
      taxon_table,
      by.x = column_attributes$name[4],
      by.y = "TAXON_NAME"
    )
  return(data)
}

filter_data <- function(data, parameter) {

  data <- dplyr::filter(data, parameter %in% parameter)
  if(nrow(data) == 0) {
    return(NULL)
  }
  return(data)
}
