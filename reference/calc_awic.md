# Acid WFD-AWIC metric

Acid WFD-AWIC metric

## Usage

``` r
calc_awic(
  data,
  names = macroinvertebrateMetrics::column_attributes$name,
  questions = c("Taxon abundance", "Taxon Abundance"),
  metric_cols = macroinvertebrateMetrics::metric_cols
)
```

## Arguments

- data:

  dataframe containing mixtaxon invertebrates

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

dataframe

## Examples

``` r
metricResults <- calc_awic(demo_data)
```
