# Riverfly metric

Function to calculate riverfly scores

## Usage

``` r
calc_riverfly(
  data,
  names = macroinvertebrateMetrics::column_attributes$name,
  questions = c("Taxon abundance", "Taxon Abundance", "Live abundance"),
  metric_cols = macroinvertebrateMetrics::metric_cols
)
```

## Arguments

- data:

  A data frame of ecology data as defined in \`column_attributes\`.

- names:

  Optional, user provided list of column names different to those used
  in \`column_attributes\` to match with input data

- questions:

  Optional, user provided 'question' default is 'Taxon abundance', which
  filters only abundance values.

- metric_cols:

  Columns used from taxon table to calculate metrics and the taxon name
  column to join to input data (if using custom taxon table)

- ...:

  Pass in specific paramters for each metric.

## Value

A data frame 5 variables

## Examples

``` r
data <- demo_data
data <- data[data$parameter == "River Family Inverts", ]
output <- calc_riverfly(data)
```
