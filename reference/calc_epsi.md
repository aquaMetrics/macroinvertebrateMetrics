# Empirically-weighted Proportion of Sediment-sensitive Invertebrates (E-PSI)

A sediment-sensitive macro-invertebrate metric that provides a proxy to
describe the extent to which the surface of river bed are composed, or
covered by sediments. It can be calculated at Taxonomic Levels 2 & 5

## Usage

``` r
calc_epsi(
  data,
  names = macroinvertebrateMetrics::column_attributes$name,
  questions = c("Taxon abundance", "Taxon Abundance"),
  taxa_list = "TL2",
  log_abundance = TRUE,
  metric_cols = macroinvertebrateMetrics::metric_cols
)
```

## Arguments

- data:

  Dataframe with at least three columns itemSAMPLE_ID - unique idenftier
  for each sample itemTAXON - Taxon name that matches to
  macroinvertebrateTaxa dataset itemRESULT - Numeric log abundance
  category

- names:

  Optional, user provided list of column names different to those used
  in \`column_attributes\` to match with input data

- questions:

  Optional, user provided 'question' default is 'Taxon abundance', which
  filters only abundance values.

- log_abundance:

  If RESULT column in ecologyResults not log category then set to FALSE.
  This will calculate log value from your numeric abundance.

- taxaList:

  The taxonomic level the sample(s) have been identified at according to
  specificed taxa lists as described in WFD100 Further Development of
  River Invertebrate Classification Tool. Either "TL2" - Taxa or "TL5" -
  Taxa list 5.

## Value

Dataframe with itemSAMPLE_ID itemANALYSIS_NAME itemDETERMINAND
itemRESULT

## References

Turley, Matt & Bilotta, Gary & Chadd, Richard & A Extence, Chris & E
Brazier, Richard & Burnside, Niall & Pickwell, Alex. (2016). A
sediment-specific family-level biomonitoring tool to identify the
impacts of fine sediment in temperate rivers and streams. Ecological
Indicators. 70. 151-165. 10.1016/j.ecolind.2016.05.040.

## Examples

``` r
sample <- demo_data
calc_epsi(data = sample, taxa_list = "TL2")
#> # A tibble: 181 × 5
#>    sample_id parameter_long                          parameter question response
#>    <chr>     <chr>                                   <chr>     <chr>    <chr>   
#>  1 1017980   Enhanced Proportion of Sediment-sensit… METRIC E… EPSI Sc… 97.2877…
#>  2 1017980   Enhanced Proportion of Sediment-sensit… METRIC E… EPSI Co… Minimal…
#>  3 1017980   Enhanced Proportion of Sediment-sensit… METRIC E… EPSI Sc… 100     
#>  4 1017980   Enhanced Proportion of Sediment-sensit… METRIC E… EPSI Co… Minimal…
#>  5 1101214   Enhanced Proportion of Sediment-sensit… METRIC E… EPSI Sc… 94.6979…
#>  6 1101214   Enhanced Proportion of Sediment-sensit… METRIC E… EPSI Co… Minimal…
#>  7 1101214   Enhanced Proportion of Sediment-sensit… METRIC E… EPSI Sc… 100     
#>  8 1101214   Enhanced Proportion of Sediment-sensit… METRIC E… EPSI Co… Minimal…
#>  9 1250462   Enhanced Proportion of Sediment-sensit… METRIC E… EPSI Sc… 97.8168…
#> 10 1250462   Enhanced Proportion of Sediment-sensit… METRIC E… EPSI Co… Minimal…
#> # ℹ 171 more rows
```
