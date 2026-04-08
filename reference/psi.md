# Proportion of Sediment-sensitive Invertebrates (PSI)

A sediment-sensitive macro-invertebrate metric that provides a proxy to
describe the extent to which the surface of river bed are composed, or
covered by sediments. It can be calculated at Taxonomic Levels 3, 4 & 5

## Usage

``` r
psi(data, taxa_list = "TL3")
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
