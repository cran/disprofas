context("Calculate f2")

test_that("get_f2_results_match", {
  res1 <- get_f2(data = dip2[dip2$batch %in% c("b0", "b1"), ],
                 ins = 1:24, tcol = 5:8, grouping = "batch")
  res2 <- get_f2(data = dip2[dip2$batch %in% c("b0", "b2"), ],
                 ins = 1:24, tcol = 5:8, grouping = "batch")
  res3 <- get_f2(data = dip2[dip2$batch %in% c("b0", "b3"), ],
                 ins = 1:24, tcol = 5:8, grouping = "batch")
  res4 <- get_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 ins = 1:24,  tcol = 5:8, grouping = "batch")
  res5 <- get_f2(data = dip2[dip2$batch %in% c("b0", "b5"), ],
                 ins = 1:24, tcol = 5:8, grouping = "batch")

  # <-><-><-><->

  expect_equal(round(res1, 2), 60.03)
  expect_equal(round(res2, 2), 51.08)
  expect_equal(round(res3, 2), 51.19)
  expect_equal(round(res4, 2), 50.07)
  expect_equal(round(res5, 2), 48.05)
})


test_that("get_f2_fails", {
  tmp0 <- dip2
  tmp0$t.30 <- as.factor(tmp0$t.30)

  tmp1 <- dip2
  tmp1$batch <- as.character(tmp1$batch)

  # <-><-><-><->

  expect_error(
    get_f2(data = as.matrix(dip2[dip2$batch %in% c("b0", "b4"), 5:8]),
           ins = 1:24, tcol = 5:8, grouping = "batch"),
    "data must be provided as data frame")
  expect_error(
    get_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = "1:24", tcol = 5:8, grouping = "batch"),
    "ins must be an integer vector")
  expect_error(
    get_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:2, tcol = 5:8, grouping = "batch"),
    "ins must be an integer vector")
  expect_error(
    get_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:25, tcol = 5:8, grouping = "batch"),
    "ins must be an integer vector")
  expect_error(
    get_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24 + 0.1, tcol = 5:8, grouping = "batch"),
    "ins must be an integer vector")
  expect_error(
    get_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = "tcol", grouping = "batch"),
    "tcol must be an integer vector")
  expect_error(
    get_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 5:6, grouping = "batch"),
    "tcol must be an integer vector")
  expect_error(
    get_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 5:8 + 0.1,
           grouping = "batch"),
    "tcol must be an integer vector")
  expect_error(
    get_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 5:9, grouping = "batch"),
    "Some columns specified by tcol")
  expect_error(
    get_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 3:8, grouping = "batch"),
    "Some names of columns specified by tcol")
  expect_error(
    get_f2(data = tmp0[tmp0$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 5:8, grouping = "batch"),
    "Some columns specified by tcol are not numeric")
  expect_error(
    get_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 5:8, grouping = 5),
    "grouping must be string")
  expect_error(
    get_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 5:8, grouping = "lot"),
    "grouping variable was not found")
  expect_error(
    get_f2(data = tmp1[tmp1$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 5:8, grouping = "batch"),
    "grouping variable's column in data")
  expect_error(
    get_f2(data = dip2[dip2$batch %in% c("b0", "b3", "b4"), ],
           ins = 1:24, tcol = 5:8, grouping = "batch"),
    "number of levels in column")
})
