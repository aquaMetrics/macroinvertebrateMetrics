context("calc_riverfly")

test_that("test riverfly scores", {
  ecology_results <- demo_data
  ecology_results <- ecology_results[
    ecology_results$parameter  == "River Family Inverts", ]
  riverfly_output <- calc_riverfly(ecology_results)

  expect_equal(sum(riverfly_output[1, column_attributes$name[3]]), 10)
  expect_equal(sum(riverfly_output[2, column_attributes$name[3]]), 7)
  expect_equal(sum(riverfly_output[3, column_attributes$name[3]]), 10)
})


test_that("test riverfly scores without question", {
  ecology_results <- demo_data
  ecology_results <- ecology_results[
    ecology_results$parameter  == "River Family Inverts", ]
  ecology_results$question <- NULL
  riverfly_output <- calc_riverfly(ecology_results)

  expect_equal(sum(riverfly_output[1, column_attributes$name[3]]), 10)
  expect_equal(sum(riverfly_output[2, column_attributes$name[3]]), 7)
  expect_equal(sum(riverfly_output[3, column_attributes$name[3]]), 10)
})

test_that("test riverfly returns nothing if no relevant data", {
  ecology_results <- demo_data
  ecology_results <- ecology_results[
    ecology_results$analysis_name == "MAC_R_TST", ]
  riverflyOutput <- calc_riverfly(ecology_results)

  expect_equal(riverflyOutput$RESULT[1], NULL)
})
