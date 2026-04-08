# Calculate Macro-invertebrate Metrics

Calculate Macro-invertebrate Metrics

## Usage

``` r
calc_metric(
  data,
  metrics = c("awic", "epsi", "psi", "riverfly", "spear", "whpt"),
  taxa_list = "TL2",
  taxon_table = macroinvertebrateMetrics::macroinvertebrateTaxa,
  names = macroinvertebrateMetrics::column_attributes$name,
  questions = c("Taxon abundance", "Taxon Abundance", "Live abundance"),
  metric_cols = macroinvertebrateMetrics::metric_cols,
  ...
)
```

## Arguments

- data:

  dataframe like \`demo_data\`

- metrics:

  One One or more of "awic", "epsi", "riverfly", "spear", "whpt".

- taxa_list:

  Taxa list either "TL2", "TL3" or "TL5.

- taxon_table:

  Optional Dataframe with WHPT scores and taxa. Default is NULL and will
  use built in WHPT scores. But you could supply custom dataframe if
  required for experimenation/development purposes.

- questions:

  Optional, user provided 'question' default is 'Taxon abundance', which
  filters only abundance values.

- metric_cols:

  Columns used from taxon table to calculate metrics plus to taxon name
  column to join to input data (if using custom taxon table)

- ...:

  Pass in specific paramters for each metric.

## Value

dataframe

## Examples

``` r
output <- calc_metric(demo_data)
```
