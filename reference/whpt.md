# Calculate WHPT scores

Calculate WHPT scores

## Usage

``` r
whpt(
  data,
  taxon_table = macroinvertebrateMetrics::macroinvertebrateTaxa,
  names = macroinvertebrateMetrics::column_attributes$name,
  questions = c("Taxon abundance", "Taxon Abundance", "Live abundance"),
  metric_cols = macroinvertebrateMetrics::metric_cols
)
```

## Arguments

- taxon_table:

  Optional Dataframe with WHPT scores and taxa. Default is NULL and will
  use built in WHPT scores. But you could supply custom dataframe if
  required for experimenation/development purposes.

- names:

  Optional, user provided list of column names different to those used
  in \`column_attributes\` to match with input data

- questions:

  Optional, user provided 'question' default is 'Taxon abundance', which
  filters only abundance values.

- ecologyResults:

  Dataframe of taxonomic results with mandatory three columns:

  SAMPLE_ID

  :   Unique sample identifier

  RESULT

  :   Numeric abundance

  TAXON

  :   Character - TL2 WHPT taxon name

  Columns names must match these exactly, but the column order does not
  matter. See demoEcologyResults for example dataset.

## Value

Dataframe with four columns: SAMPLE_ID, DETERMINAND, RESULT,
ANALYSIS_NAME, ANALYSIS_REPNAME

## Examples

``` r
metricResults <- calc_whpt(demo_data)
```
