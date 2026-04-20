# Proportion of Sediment-sensitive Invertebrates (PSI)

A sediment-sensitive macro-invertebrate metric that provides a proxy to
describe the extent to which the surface of river bed are composed, or
covered by sediments. It can be calculated at Taxonomic Levels 3, 4 & 5

## Usage

``` r
calc_psi(
  data,
  names = macroinvertebrateMetrics::column_attributes$name,
  questions = c("Taxon abundance", "Taxon Abundance"),
  taxa_list = "TL3",
  log_abundance = TRUE,
  metric_cols = macroinvertebrateMetrics::metric_cols
)
```

## Arguments

- data:

  Dataframe with at least three columns itemsample_id - unique idenftier
  for each sample itemlabel - Taxon name that matches to
  macroinvertebrateTaxa dataset itemresponse - Log abundance category

- taxa_list:

  The taxonomic level the sample(s) have been identified at according to
  specificed taxa lists as described in WFD100 Further Development of
  River Invertebrate Classification Tool. Either "TL3" - Taxa list 3,
  "TL4" - Taxa list 4 or "TL5" - Taxa list 5.

## Value

Dataframe with itemsample_id itemparmaeter itemquestion itemresponse

## References

Extence, Chris & Chadd, Richard & England, Judy & Dunbar, M.J. & Wood,
Paul & Taylor, E.D.. (2010). The Assessment of Fine Sediment
Accumulation in Rivers Using Macro-Invertebrate Community Response.
River Research and Applications. 29. 10.1002/rra.1569.

## Examples

``` r
sample <- demo_data
calc_psi(data = sample, taxa_list = "TL3")
#> # A tibble: 188 × 5
#>    sample_id parameter_long                          parameter question response
#>    <chr>     <chr>                                   <chr>     <chr>    <chr>   
#>  1 1017980   Proportion of Sediment-sensitive Inver… River Fa… PSI Sco… 69.2307…
#>  2 1017980   Proportion of Sediment-sensitive Inver… River Fa… PSI Con… Slightl…
#>  3 1017980   Proportion of Sediment-sensitive Inver… TL5 Rive… PSI Sco… 72.4137…
#>  4 1017980   Proportion of Sediment-sensitive Inver… TL5 Rive… PSI Con… Slightl…
#>  5 1101214   Proportion of Sediment-sensitive Inver… River Fa… PSI Sco… 64      
#>  6 1101214   Proportion of Sediment-sensitive Inver… River Fa… PSI Con… Slightl…
#>  7 1101214   Proportion of Sediment-sensitive Inver… TL5 Rive… PSI Sco… 74.0740…
#>  8 1101214   Proportion of Sediment-sensitive Inver… TL5 Rive… PSI Con… Slightl…
#>  9 1250462   Proportion of Sediment-sensitive Inver… River Fa… PSI Sco… 67.8571…
#> 10 1250462   Proportion of Sediment-sensitive Inver… River Fa… PSI Con… Slightl…
#> # ℹ 178 more rows
```
