context("calcWhpt")

test_that("AWIC scores match previously calculated scores in demo dataset", {
  # Create a data frame using `tribble` - tidyverse way to create a data frame (row
  # by row).
  # Create values for all the species we can expect (need to make sure we have
  # correct scores for all species and abundance categories)
  data <- tibble::tribble(
    ~response, ~label,
    1, "Agapetus",
    1, "Caenis",
    1, "Alainites muticus",
    1, "Glossosoma",
    1, "Potamopyrgus jenkinsi",
    1, "Gammarus pulex",
    1, "Perla bipunctata",
    1, "Ancylus fluviatilis",
    1, "Philopotamus montanus",
    1, "Silo pallipes",
    1, "Wormaldia",
    1, "Hydropsyche instabilis",
    1, "Ecdyonurus",
    1, "Rhithrogena",
    1, "Hydraena gracilis",
    1, "Sericostoma personatum",
    1, "Heptagenia sulphurea",
    1, "Atherix",
    1, "Esolus parallelepipedus",
    1, "Baetis rhodani",
    1, "Perlodes microcephala",
    1, "Lepidostoma hirtum",
    1, "Diplectrona felix",
    1, "Electrogena lateralis",
    1, "Hydropsyche siltalai",
    1, "Hydropsyche pellucidula",
    1, "Nigrobaetis niger",
    1, "Elmis aenea",
    1, "Chloroperla tripunctata",
    1, "Limnius volckmari",
    1, "Crenobia alpina",
    1, "Cordulegaster boltonii",
    1, "Isoperla grammatica",
    1, "Brachyptera risi",
    1, "Rhyacophila dorsalis",
    1, "Phagocata vitta",
    1, "Chloroperla torrentium",
    1, "Leuctra inermis",
    1, "Oulimnius",
    1, "Amphinemura sulcicollis",
    1, "Protonemura",
    1, "Leuctra nigra",
    1, "Leuctra hippopus",
    1, "Leptophlebia marginata",
    1, "Sialis",
    1, "Diura bicaudata",
    1, "Nemoura",
    1, "Nemurella picteti"
  )

  # Need to add in other columns our function expects to find in the data
  data$sample_id <- 1
  data$parameter <- "TL5 River Invertberate"
  data$question <- "Taxon abundance"

  # Run the data through the function
  test <- calc_awic(data)
  # Convert all responses to numeric before testing
  test$response <- as.numeric(test$response)
  # Use `expect_equal()` function from the testthat package
  testthat::expect_equal(
    round(test$response[test$question == "wfd_awic"], 2), 8.19
  )
  expect_equal(test$response[test$question == "ntaxa"], 48)
  expect_equal(test$response[test$question == "sample_score"], 393)

  # Test against Abundance category > 9 (in case of any copy paste / typos)
  data$response <- 10
  test_10 <- calc_awic(data)
  # Convert all responses to numeric before testing
  test_10$response <- as.numeric(test_10$response)
  expect_equal(
    round(test_10$response[test_10$question == "wfd_awic"], 2), 8.48
  )
  expect_equal(test_10$response[test_10$question == "ntaxa"], 48)
  expect_equal(test_10$response[test_10$question == "sample_score"], 407)

  # Test against Abundance category > 99
  data$response <- 100
  test_100 <- calc_awic(data)
  # Convert all responses to numeric before testing
  test_100$response <- as.numeric(test_100$response)
  expect_equal(
    round(test_100$response[test_100$question == "wfd_awic"], 2), 8.77
  )
  expect_equal(test_100$response[test_100$question == "ntaxa"], 48)
  expect_equal(test_100$response[test_100$question == "sample_score"], 421)
})
