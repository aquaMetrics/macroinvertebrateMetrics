# Filter data before calculating SPEAR metric

Filter data before calculating SPEAR metric

## Usage

``` r
filter_spear(data, taxa_list = NULL)
```

## Arguments

- data:

  Dataframe with ecology results...

- taxa_list:

  The taxonomic level the sample(s) have been identified at according to
  specified taxa lists as described in WFD100 Further Development of
  River Invertebrate Classification Tool. Either "TL2" - Taxa List 2,
  "TL4" - Taxa List 4 or "TL5" - Taxa List 5.

## Value

Dataframe of filtered and aggregated results (based on Taxa List) with
four columns: sample_id, label, SPEAR_SPECIES, response
