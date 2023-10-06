
<!-- README.md is generated from README.Rmd. Please edit that file -->

# macroinvertebrateMetrics

<img src='https://github.com/aquaMetrics/macroinvertebrateMetrics/blob/master/man/figures/macro_logo.png?raw=true' align="right" width="200" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/aquaMetrics/macroinvertebrateMetrics/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aquaMetrics/macroinvertebrateMetrics/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/aquaMetrics/macroinvertebrateMetrics/branch/master/graph/badge.svg?token=sUCV2wxoHI)](https://codecov.io/gh/aquaMetrics/macroinvertebrateMetrics)
<!-- badges: end -->

# Work in Progress

Please don’t use in production

# aquaMetrics package

An R package to calculate macroinverebrate metrics

In R: Install the development version from GitHub:  
`install.packages("devtools")`  
`library(devtools)`  
`install_github("aquaMetrics/macroinvertebrateMetrics")`

# Quick-start

Calculate all metrics:

``` r
library(macroinvertebrateMetrics)
all_metrics <- calc_metric(demo_data)
head(all_metrics, 4)
#> # A tibble: 4 x 5
#>   sample_id question     response         parameter parameter_long
#>   <chr>     <chr>        <chr>            <chr>     <chr>         
#> 1 1017980   sample_score 67               WFD_AWIC  WFD AWIC      
#> 2 1017980   ntaxa        7                WFD_AWIC  WFD AWIC      
#> 3 1017980   wfd_awic     9.57142857142857 WFD_AWIC  WFD AWIC      
#> 4 1101214   sample_score 66               WFD_AWIC  WFD AWIC
```

Calculate specific metrics:

``` r

selected_metrics <- calc_metric(demo_data, metrics = c("whpt","awic"))
head(selected_metrics, 4)
#> # A tibble: 4 x 5
#>   sample_id question   response parameter   parameter_long
#>   <chr>     <chr>      <chr>    <chr>       <chr>         
#> 1 1017980   WHPT_SCORE 82.4     WHPT Metric WHPT Metric   
#> 2 1101214   WHPT_SCORE 68.3     WHPT Metric WHPT Metric   
#> 3 1250462   WHPT_SCORE 81.5     WHPT Metric WHPT Metric   
#> 4 1419451   WHPT_SCORE 70.4     WHPT Metric WHPT Metric
```

Use metric specific function:

``` r

specific_metric <- calc_epsi(demo_data)
head(specific_metric, 4)
#> # A tibble: 4 x 5
#>   sample_id parameter_long                           parameter question response
#>   <chr>     <chr>                                    <chr>     <chr>    <chr>   
#> 1 1017980   Enhanced Proportion of Sediment-sensiti~ METRIC E~ EPSI Sc~ 97.3177~
#> 2 1017980   Enhanced Proportion of Sediment-sensiti~ METRIC E~ EPSI Co~ Minimal~
#> 3 1101214   Enhanced Proportion of Sediment-sensiti~ METRIC E~ EPSI Sc~ 94.7977~
#> 4 1101214   Enhanced Proportion of Sediment-sensiti~ METRIC E~ EPSI Co~ Minimal~
```

# Concepts

To calculate any metric you need:

## Input Data

- The absolute minimium input data is a dataframe with at least two
  columns containing: `label` (taxon name) and `response` (abundance -
  some metrics don’t require abundance).
- Optional `sample_id`, `parameter`, `question` - these book-keeping
  variables give more info on the type of data and metrics which can be
  calculated based on the input data.

For example:

| sample_id | label         | response | parameter            |
|:----------|:--------------|:---------|:---------------------|
| 3201863   | Baetidae      | 53       | River Family Inverts |
| 3201863   | Hydrobiidae   | 2        | River Family Inverts |
| 3201863   | Sphaeriidae   | 2        | River Family Inverts |
| 3201863   | Erpobdellidae | 1        | River Family Inverts |

Here’s the precise definition of each recommended input column see
`column_attributes`:

| name           | col_type  | require | description                                                                             |
|:---------------|:----------|:--------|:----------------------------------------------------------------------------------------|
| sample_id      | character | TRUE    | Unique sample id for grouping metric outputs at a sample level                          |
| question       | character | FALSE   | Question being posed such as ‘abundance’                                                |
| response       | numeric   | TRUE    | Response received to the question                                                       |
| label          | character | TRUE    | The ‘label’ sometimes given to question such as species name                            |
| parameter      | character | TRUE    | Parameter is the method used to respond to the question for instance ‘Family level TL2’ |
| parameter_long | character | FALSE   | Long name of paramater for example ‘Lab anlaysis to family level Taxa List 2’           |

- The parameter gives info of the method of analysis used. This is
  usually the taxonomic level of identification. Here’s the list of
  recognised analyses:

<!-- -->

    #> [1] "TL5 River Invertebrate" "River Family Inverts"   "BANKSIDE_INVERTS"      
    #> [4] "Work in progress"       "TL3 River Invertebrate"

- A TL5 sample can be downgraded to TL3 or TL2 ‘family’ level.
- Depending on the Taxonomic Level of the sample selected only certain
  metrics can be calculated
- The list of which metric can be calculated at which level is listed
  here:

| metric   | parameter              |
|:---------|:-----------------------|
| awic     | TL5 River Invertebrate |
| epsi     | TL5 River Invertebrate |
| epsi     | River Family Inverts   |
| epsi     | BANKSIDE_INVERTS       |
| lamm     | TL5 River Invertebrate |
| life     | Work in progress       |
| psi      | River Family Inverts   |
| psi      | TL3 River Invertebrate |
| psi      | TL5 River Invertebrate |
| psi      | BANKSIDE_INVERTS       |
| riverfly | River Family Inverts   |
| spear    | TL5 River Invertebrate |
| spear    | River Family Inverts   |
| spear    | BANKSIDE_INVERTS       |
| whpt     | River Family Inverts   |
| whpt     | BANKSIDE_INVERTS       |

- If parameter is not supplied, all metrics will be calculated however,
  this may not be appropriate depending on what taxonomic resolution
  your data was analysed to.

## Input Metadata

- For each metric a table identifying the column names used for
  sample_id, taxon_name and abundance
- A list of validation rules for each column in the input data
- See ‘column_attributes’ for example

| name      | col_type  | require | description                                                    |
|:----------|:----------|:--------|:---------------------------------------------------------------|
| sample_id | character | TRUE    | Unique sample id for grouping metric outputs at a sample level |
| question  | character | FALSE   | Question being posed such as ‘abundance’                       |
| response  | numeric   | TRUE    | Response received to the question                              |
| label     | character | TRUE    | The ‘label’ sometimes given to question such as species name   |

## Metric scores

- A list of scores given to each taxa based on expert opinion or some
  relationship with pressure
- See `macroinvertebrateTaxa`

For example:

|     | TAXON_NAME     | WHPT_A | EPSI_WEIGHT_FAM | SPEAR_SPECIES |
|:----|:---------------|-------:|----------------:|:--------------|
| 129 | Planariidae    |    4.7 |            0.25 | FALSE         |
| 143 | Dugesiidae     |    2.8 |            0.00 | FALSE         |
| 151 | Dendrocoelidae |    3.0 |            0.01 | FALSE         |
| 974 | Neritidae      |    6.4 |            0.48 | FALSE         |

## Metric Metadata

- See `metric_cols`
- The name of the metric
- The names of the columns required to calculate a particular metric

| metric | metric_names    | parameter              |
|:-------|:----------------|:-----------------------|
| awic   | AWIC_A          | TL5 River Invertebrate |
| awic   | AWIC_B          | TL5 River Invertebrate |
| awic   | AWIC_C          | TL5 River Invertebrate |
| epsi   | EPSI_WEIGHT_TL5 | TL5 River Invertebrate |

## Calculate

There are two ways to calculate a given metric, for example WHPT can be
calculated using either `calc_metric(demo_data, metric = "whpt")` or the
metric specific function `calc_whpt(demo_data)`. These two function
execute the same code except that `calc_awic()` uses `calc_metric()`
behind the scenes and hard codes the `metric = "whpt"` parameter to save
a little bit of typing.

All metrics follow the same calculation process:

1.  The `data` input is passed to a `calc_metric()` function.

2.  Input `data` is validated in an internal function called
    `validate_data()`. This function checks the data is in the correct
    format and then joins the data to taxonomic table containing metrics
    values.

3.  The `data` (now with the metric values attached) is then passed to
    specific internal metric functions. These metric function calculates
    the specific metrics. If more then one metric is requested, the
    `calc_metric()` function calculates only the requested metrics
    in-turn and then binds and returns the outputs in a final output
    dataframe.
