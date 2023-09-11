
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

- Usually dataframe with at least three columns: sample_id, taxon name
  and abundance

## Input Data Metadata

- For each metric a table identifying the column names used for
  sample_id, taxon_name and abundance
- A list of validation rules for each column in the input data
- See ‘column_attributes’

## Metric scores

- A list of scores given to each taxa based on expert opinion or some
  relationship with pressure

## Metric Scores Metadata

- See ‘metric_cols’
- The name of the metric
- The names of the columns required to calculate a particular metric
