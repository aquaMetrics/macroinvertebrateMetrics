# Riverfly metric

Function to calculate riverfly scores

## Usage

``` r
riverfly(
  data,
  names = macroinvertebrateMetrics::column_attributes$name,
  questions = c("Taxon abundance", "Taxon Abundance", "Live abundance"),
  metric_cols = metric_cols
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

## Value

A data frame 5 variables

## Examples

``` r
data <- demo_data
data <- data[data$parameter == "River Family Inverts", ]
output <- calc_riverfly(data)
```
