% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc-epsi.R
\name{calc_epsi}
\alias{calc_epsi}
\title{Empirically-weighted Proportion of Sediment-sensitive Invertebrates (E-PSI)}
\usage{
calc_epsi(
  data,
  names = macroinvertebrateMetrics::column_attributes$name,
  questions = c("Taxon abundance", "Taxon Abundance"),
  taxa_list = "TL2",
  log_abundance = TRUE,
  metric_cols = macroinvertebrateMetrics::metric_cols
)
}
\arguments{
\item{data}{Dataframe with at least three columns
item{SAMPLE_ID} - unique idenftier for each sample
item{TAXON} - Taxon name that matches to macroinvertebrateTaxa dataset
item{RESULT} - Numeric log abundance category}

\item{names}{Optional, user provided list of column names different to those used
in `column_attributes` to match with input data}

\item{questions}{Optional, user provided 'question' default is 'Taxon abundance', which
filters only abundance values.}

\item{log_abundance}{If RESULT column in ecologyResults not log category then
set to FALSE. This will calculate log value from your numeric abundance.}

\item{taxaList}{The taxonomic level the sample(s) have been identified at
according to specificed taxa lists as described in WFD100 Further
Development of River Invertebrate Classification Tool. Either "TL2" - Taxa
or  "TL5" - Taxa list 5.}
}
\value{
Dataframe with
item{SAMPLE_ID}
item{ANALYSIS_NAME}
item{DETERMINAND}
item{RESULT}
}
\description{
A sediment-sensitive macro-invertebrate metric that provides a proxy to
describe the extent to which the surface of river bed are composed, or
covered by sediments. It can be calculated at Taxonomic Levels 2 & 5
}
\examples{
sample <- demo_data
calc_epsi(data = sample, taxa_list = "TL2")
}
\references{
Turley, Matt & Bilotta, Gary & Chadd, Richard & A Extence, Chris & E Brazier,
Richard & Burnside, Niall & Pickwell, Alex. (2016). A sediment-specific
family-level biomonitoring tool to identify the impacts of fine sediment in
temperate rivers and streams. Ecological Indicators. 70. 151-165.
10.1016/j.ecolind.2016.05.040.
}
