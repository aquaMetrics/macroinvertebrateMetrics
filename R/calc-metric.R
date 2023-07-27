#' Calculate Macro-invertebrate Metrics
#'
#' @param data dataframe like `demoEcologyResults`
#' @param metrics One One or more of "awic", "epsi", "riverfly", "spear", "whpt".
#' @param taxa_list Taxa list either "TL2", "TL3" or "TL5.
#'
#' @return dataframe
#' @export
#'
#' @examples
#' output <- calc_metric(demoEcologyResults)
calc_metric <- function(data,
                        metrics = c(
                          "awic",
                          "epsi",
                          "riverfly",
                          "spear",
                          "whpt"
                        ),
                        taxa_list = "TL2") {
  output <- purrr::map_df(metrics, function(metric) {
    if (any(metric %in% c("awic"))) {
      awic <- calc_awic(data)
      awic$RESULT <- as.character(awic$RESULT)
      return(awic)
    }

    if (any(metric %in% c("epsi"))) {
      epsi <- calc_epsi(data, taxaList = taxa_list)
      return(epsi)
    }
    if (any(metric %in% c("riverfly"))) {
      riverfly <- calc_riverfly(data)
      riverfly$RESULT <- as.character(riverfly$RESULT)
      return(riverfly)
    }
    if (any(metric %in% c("spear"))) {
      # These metrics need specific Taxa List to run correctly
      spear_data <- filter_spear(data, taxaList = taxa_list)
      if (nrow(spear_data) > 0) {
        spear <- calc_spear(spear_data)
        return(spear)
      } else {
        return(NULL)
      }
    }
    if (any(metric %in% c("whpt"))) {
      whpt <- calc_whpt(data)
      whpt$RESULT <- as.character(whpt$RESULT)
      return(whpt)
    }
  })
  return(output)
}
