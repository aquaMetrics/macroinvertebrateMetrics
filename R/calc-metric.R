#' Calculate Macro-invertebrate Metrics
#'
#' @param data dataframe like `demo_data`
#' @param metrics One One or more of "awic", "epsi", "riverfly", "spear", "whpt".
#' @param taxa_list Taxa list either "TL2", "TL3" or "TL5.
#' @param questions
#' Optional, user provided 'question' default is 'Taxon abundance', which
#' filters only abundance values.
#' @param taxon_table
#' Optional Dataframe with WHPT scores and taxa. Default is NULL and will use
#' built in WHPT scores. But you could supply custom dataframe if required for
#' experimenation/development purposes.
#' @param metric_cols Columns used from taxon table to calculate metrics plus to
#'   taxon name column to join to input data (if using custom taxon table)
#' @param ... Pass in specific paramters for each metric.
#' @return dataframe
#' @export
#'
#' @examples
#' output <- calc_metric(demo_data)
calc_metric <- function(
    data,
    metrics = c(
      "awic",
      "epsi",
      "psi",
      "riverfly",
      "spear",
      "whpt"
    ),
    taxa_list = "TL2",
    taxon_table = macroinvertebrateMetrics::macroinvertebrateTaxa,
    names = macroinvertebrateMetrics::column_attributes$name,
    questions = c(
      "Taxon abundance",
      "Taxon Abundance",
      "Live abundance"
    ),
    metric_cols = macroinvertebrateMetrics::metric_cols,
    ...) {
  # To allow user to specify the names of the columns to match the columns in
  # their dataset update package column name data with column names provided to
  # function
  column_attributes <- macroinvertebrateMetrics::column_attributes
  column_attributes$name <- names
  # Validate and format input data - column names are user provided or revert to
  # default. After this point, columns names are referred by index/number rather
  # than text of column name, this allows the default column names to be update
  # easily in future
  data <- validate_input(
    data = data,
    names = names,
    questions = questions,
    taxon_table = taxon_table,
    metric_cols = metric_cols
  )

  output <- purrr::map_df(metrics, function(metric) {
    metric_cols <- metric_cols[metric_cols$metric == metric, ]
    # filter for correct Taxa List(s)/Parameter level for metric
    filtered_data <- filter_data(data, parameter = unique(metric_cols$parameter))
    if(is.null(filtered_data)) {
      return(NULL)
    }
    if (any(metric %in% "awic")) {
      awic <- awic(filtered_data,
                   metric_cols = metric_cols,
                   names = names)
      return(awic)
    }

    if (any(metric %in% "epsi")) {
      epsi <- epsi(filtered_data, taxa_list = taxa_list, metric_cols = metric_cols, ...)
      return(epsi)
    }
    if (any(metric %in% "psi")) {
      # These metrics need specific Taxa List to run correctly
      psi_data <- filter_psi(filtered_data, taxa_list = taxa_list)
      whpt <- psi(psi_data)
      return(whpt)
    }
    if (any(metric %in% "riverfly")) {
      riverfly <- riverfly(filtered_data)
      return(riverfly)
    }
    if (any(metric %in% "spear")) {
      # These metrics need specific Taxa List to run correctly
      spear_data <- filter_spear(filtered_data, taxa_list = taxa_list)
      spear <- spear(spear_data)
      return(spear)
    }
    if (any(metric %in% "whpt")) {
      whpt <- whpt(filtered_data)
      return(whpt)
    }
  })
  return(output)
}
