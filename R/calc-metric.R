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
    # Make sample_id include parameter id so if parameters share same sample_id
    # they will be calculated separately
    filtered_data$sample_id <- paste0(filtered_data$sample_id,"-",
                                      filtered_data$parameter)
    sample_output <- purrr::map_df(
      split(filtered_data, filtered_data$sample_id), function(sample) {
    if (any(metric %in% "awic")) {
      metric_output <- awic(sample,
                   metric_cols = metric_cols,
                   names = names)
    }

    if (any(metric %in% "epsi")) {
      metric_output <- epsi(sample,
                   taxa_list = taxa_list,
                   metric_cols = metric_cols,
                   ...)
    }
    if (any(metric %in% "psi")) {
      # These metrics need specific Taxa List to run correctly
      psi_data <- filter_psi(sample, taxa_list = taxa_list)
      metric_output <- psi(psi_data)
    }
    if (any(metric %in% "riverfly")) {
      metric_output <- riverfly(sample)
    }
    if (any(metric %in% "spear")) {
      # These metrics need specific Taxa List to run correctly
      spear_data <- filter_spear(sample, taxa_list = taxa_list)
      metric_output <- spear(spear_data)
    }
    if (any(metric %in% "whpt")) {
      metric_output <- whpt(sample)
    }

    if(exists("metric_output")) {
      if(is.na(unique(sample$parameter)) == FALSE){
        metric_output$parameter <-  paste0(metric_output$parameter, " ", unique(sample$parameter))
      }
    }
    return(metric_output)
    # Unique parameter name to identify analysis/parameter in output

  })
   return(sample_output)
  })
  output$sample_id <- gsub("-.*", "", output$sample_id)
  return(output)
}
