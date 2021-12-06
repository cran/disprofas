context("Calculate f1")

test_that("get_f1_results_match", {
  res1 <- get_f1(data = dip2[dip2$batch %in% c("b0", "b1"), ],
                 ins = 1:24, tcol = 5:8, grouping = "batch")
  res2 <- get_f1(data = dip2[dip2$batch %in% c("b0", "b2"), ],
                 ins = 1:24, tcol = 5:8, grouping = "batch")
  res3 <- get_f1(data = dip2[dip2$batch %in% c("b0", "b3"), ],
                 ins = 1:24, tcol = 5:8, grouping = "batch")
  res4 <- get_f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 ins = 1:24,  tcol = 5:8, grouping = "batch")
  res5 <- get_f1(data = dip2[dip2$batch %in% c("b0", "b5"), ],
                 ins = 1:24, tcol = 5:8, grouping = "batch")

  # <-><-><-><->

  expect_equal(signif(res1, 7), 8.729032)
  expect_equal(signif(res2, 7), 13.20579)
  expect_equal(signif(res3, 7), 13.67089)
  expect_equal(signif(res4, 7), 7.379616)
  expect_equal(signif(res5, 7), 13.94996)
})

test_that("get_f1_fails", {
  tmp0 <- dip2
  tmp0$t.30 <- as.factor(tmp0$t.30)

  tmp1 <- dip2
  tmp1$batch <- as.character(tmp1$batch)

  # <-><-><-><->

  expect_error(
    get_f1(data = as.matrix(dip2[dip2$batch %in% c("b0", "b4"), 5:8]),
           ins = 1:24, tcol = 5:8, grouping = "batch"),
    "data must be provided as data frame")
  expect_error(
    get_f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = "1:24", tcol = 5:8, grouping = "batch"),
    "ins must be an integer vector")
  expect_error(
    get_f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:2, tcol = 5:8, grouping = "batch"),
    "ins must be an integer vector")
  expect_error(
    get_f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:25, tcol = 5:8, grouping = "batch"),
    "ins must be an integer vector")
  expect_error(
    get_f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24 + 0.1, tcol = 5:8, grouping = "batch"),
    "ins must be an integer vector")
  expect_error(
    get_f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = "tcol", grouping = "batch"),
    "tcol must be an integer vector")
  expect_error(
    get_f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 5:6, grouping = "batch"),
    "tcol must be an integer vector")
  expect_error(
    get_f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 5:8 + 0.1,
           grouping = "batch"),
    "tcol must be an integer vector")
  expect_error(
    get_f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 5:9, grouping = "batch"),
    "Some columns specified by tcol")
  expect_error(
    get_f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 3:8, grouping = "batch"),
    "Some names of columns specified by tcol")
  expect_error(
    get_f1(data = tmp0[tmp0$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 5:8, grouping = "batch"),
    "Some columns specified by tcol are not numeric")
  expect_error(
    get_f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 5:8, grouping = 5),
    "grouping must be string")
  expect_error(
    get_f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 5:8, grouping = "lot"),
    "grouping variable was not found")
  expect_error(
    get_f1(data = tmp1[tmp1$batch %in% c("b0", "b4"), ],
           ins = 1:24, tcol = 5:8, grouping = "batch"),
    "grouping variable's column in data")
  expect_error(
    get_f1(data = dip2[dip2$batch %in% c("b0", "b3", "b4"), ],
           ins = 1:24, tcol = 5:8, grouping = "batch"),
    "number of levels in column")
})
