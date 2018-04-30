context("apply linting rules")

# install this if required: devtools::install_github("jimhester/lintr")
if (requireNamespace("lintr", quietly = TRUE)) {
  context("test linting")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}
