# Filter results for PSI metric

Filter results for PSI metric

## Usage

``` r
filter_psi(data, taxa_list = taxa_list)
```

## Arguments

- data:

  Dataframe of ecology results with mandatory four columns:

  - response - Numeric count result.

  - label - Taxon name matching names in
    macroinvertebrates::macroinvertebrateTaxa table

  - question - 'Taxon abundance'

  - sample_id - Unique identifier for each sample (number or text)

- taxa_list:

  The taxonomic level the sample(s) have been identified at according to
  specified taxa lists as described in WFD100 Further Development of
  River Invertebrate Classification Tool. Either "TL2" - Taxa list 2,
  "TL3" - Taxa list 2 or "TL5" Taxa list 5 or "TL4". PSI TL lists don't
  match RIVPACS species e.g. TL3 list includes 'Oligochaeta' even though
  it is not a TL3 taxa according to WFD100.

## Value

Dataframe
