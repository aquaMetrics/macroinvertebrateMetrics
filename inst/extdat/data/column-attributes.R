column_attributes <- tibble(
  name = c(
    "sample_id",
    "question",
    "response",
    "label",
    "parameter",
    "parameter_long"
  ),
  col_type =
    c(
      "character",
      "character",
      "numeric",
      "character",
      "character",
      "character"
    ),
  require =
    c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE),
  description =
    c(
      "Unique sample id for grouping metric outputs at a sample level",
      "Question being posed such as 'abundance' ",
      "Response received to the question",
      "The 'label' sometimes given to question such as species name",
      "Parameter is the method used to respond to the question for instance 'Family level TL2'",
      "Long name of paramater for example 'Lab anlaysis to family level Taxa List 2'"
    )
)
