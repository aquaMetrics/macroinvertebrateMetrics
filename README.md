
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

# Concepts

To calculate any metric you need:

## Input Data

- The absolute minimium input data is a dataframe with at least two
  columns containing: taxon name and abundance\*
- \*some metrics don’t require abundance.
- Optional sample_id, parameter, question - these well book keep and
  give more precise on the type of data and metrics which can be
  calculated based on the input data.

For example:

    #> # A tibble: 4 x 4
    #>   sample_id label         response parameter           
    #>   <chr>     <fct>         <chr>    <chr>               
    #> 1 3201863   Baetidae      53       River Family Inverts
    #> 2 3201863   Hydrobiidae   2        River Family Inverts
    #> 3 3201863   Sphaeriidae   2        River Family Inverts
    #> 4 3201863   Erpobdellidae 1        River Family Inverts

## Input Metadata

- For each metric a table identifying the column names used for
  sample_id, taxon_name and abundance
- A list of validation rules for each column in the input data
- See ‘column_attributes’ for example

<!-- -->

    #> # A tibble: 4 x 4
    #>   name      col_type  require description                                       
    #>   <chr>     <chr>     <lgl>   <chr>                                             
    #> 1 sample_id character TRUE    "Unique sample id for grouping metric outputs at ~
    #> 2 question  character FALSE   "Question being posed such as 'abundance' "       
    #> 3 response  numeric   TRUE    "Response received to the question"               
    #> 4 label     character TRUE    "The 'label' sometimes given to question such as ~

## Metric scores

- A list of scores given to each taxa based on expert opinion or some
  relationship with pressure

For example:

    #>         TAXON_NAME WHPT_A EPSI_WEIGHT_FAM SPEAR_SPECIES
    #> 129    Planariidae    4.7            0.25         FALSE
    #> 143     Dugesiidae    2.8            0.00         FALSE
    #> 151 Dendrocoelidae    3.0            0.01         FALSE
    #> 974      Neritidae    6.4            0.48         FALSE

## Metric Metadata

- See ‘metric_cols’
- The name of the metric
- The names of the columns required to calculate a particular metric

<!-- -->

    #>   metric    metric_names
    #> 1   awic          AWIC_A
    #> 2   awic          AWIC_B
    #> 3   awic          AWIC_C
    #> 4   epsi EPSI_WEIGHT_TL5
