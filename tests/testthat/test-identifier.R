testthat::test_that("Outputs are strings", {
  dat <- data.frame(stats::runif(50), stats::runif(50))
  x <- make_ids(dat)
  checkmate::qexpect(x, "S50")
})

testthat::test_that("Outputs are unique", {
  dat <- data.frame(stats::runif(50), stats::runif(50))
  x <- make_ids(dat)
  testthat::expect_false(any(duplicated(x)))
})

testthat::test_that("Complaint on invalid width", {
  dat <- data.frame(stats::runif(50), stats::runif(50))
  testthat::expect_warning(make_ids(dat, num_digits = 1))
})
