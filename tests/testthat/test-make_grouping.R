context("Make grouping")

test_that("make_grouping_succeeds", {
  res1 <- make_grouping(dip1, grouping = "type")
  res2 <- make_grouping(data = dip2[dip2$batch %in% c("b0", "b1"), ],
                         grouping = "batch")

  # <-><-><-><->

  expect_equal(sum(res1), 6)
  expect_equal(sum(!res1), 6)
  expect_equal(sum(res1[1:6] == rep(TRUE, 6)), 6)

  expect_equal(sum(res2), 12)
  expect_equal(sum(!res2), 12)
  expect_equal(sum(res2[1:12] == rep(TRUE, 12)), 12)
})

test_that("make_grouping_fails", {
  tmp <- dip1
  tmp$type <-  as.character(tmp$type)

  # <-><-><->

  expect_error(make_grouping(data = as.matrix(dip1[, 3:9]), grouping = "batch"),
               "data must be provided as data frame")
  expect_error(make_grouping(data = dip1, grouping = 5),
               "grouping must be string")
  expect_error(make_grouping(data = dip1, grouping = "lot"),
               "grouping variable was not found")
  expect_error(make_grouping(data = dip1, grouping = "t.5"),
               "column in data specified by grouping")
})
