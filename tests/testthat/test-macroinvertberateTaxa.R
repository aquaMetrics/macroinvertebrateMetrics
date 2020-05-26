context("test macroinvertebrateTaxa")

test_that("Test .csv source file matches binary file", {
  # These two files should be the same, there are two files for ease of use:
  # 1. "INVERT-TAXON-DICTIONARY" is in csv file for easy reading and
  # comparison on github i.e. this is plain text and easy to track changes

  taxonDictionarySource <- utils::read.csv(
    system.file("extdata",
                "invert-taxon-dictionary.csv",
                package = "macroinvertebrateMetrics"))
  # 2. "macroinvertebrateTaxa" table is held in sys.data file as a binary for fast access in
  # the package
  taxonDictionaryBinary <- macroinvertebrateTaxa
  # check that the binary file matches the plain text csv file
  matches <- all.equal(taxonDictionarySource,
                          taxonDictionaryBinary)
  expect_equal(matches[1], TRUE)
  # If this test fails, check if either file has been changed. Changes in
  # taxonomy / scores should flow from taxonDictionarySource to
  # taxonDictionaryBinary - if changes agreed by all
  # Follow guidance for updating the macroinvertebrateTaxa binary data:
  # http://r-pkgs.had.co.nz/data.html#data-sysdata
})
