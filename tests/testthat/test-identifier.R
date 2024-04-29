test_that("Outputs are strings", {
  dat <- data.frame(runif(50), runif(50))
  x <- make_ids(dat)
  checkmate::qexpect(x, "S50")
})

test_that("Outputs are unique", {
  dat <- data.frame(runif(50), runif(50))
  x <- make_ids(dat)
  expect_false(any(duplicated(x)))
})

test_that("Complaint on invalid width", {
  dat <- data.frame(runif(50), runif(50))
  expect_warning(make_ids(dat, num_digits = 1))
})
