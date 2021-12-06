context("f1")

test_that("f1_results_match", {
  l_res1 <- f1(data = dip2[dip2$batch %in% c("b0", "b1"), ],
               tcol = 5:8, grouping = "batch", use_EMA = "no",
               bounds = c(1, 100))
  l_res2 <- f1(data = dip2[dip2$batch %in% c("b0", "b2"), ],
               tcol = 5:8, grouping = "batch", use_EMA = "no",
               bounds = c(1, 100))
  l_res3 <- f1(data = dip2[dip2$batch %in% c("b0", "b3"), ],
               tcol = 5:8, grouping = "batch", use_EMA = "no",
               bounds = c(1, 100))
  l_res4 <- f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
               tcol = 5:8, grouping = "batch", use_EMA = "no",
               bounds = c(1, 100))
  l_res5 <- f1(data = dip2[dip2$batch %in% c("b0", "b5"), ],
               tcol = 5:8, grouping = "batch", use_EMA = "no",
               bounds = c(1, 100))

  # <-><-><-><->

  expect_equal(signif(l_res1$f1, 7), 8.729032)
  expect_equal(signif(l_res2$f1, 7), 13.20579)
  expect_equal(signif(l_res3$f1, 7), 13.67089)
  expect_equal(signif(l_res4$f1, 7), 7.379616)
  expect_equal(signif(l_res5$f1, 7), 13.94996)
})

test_that("f1_fails", {
  tmp0 <- dip2
  tmp0$t.30 <- as.factor(tmp0$t.30)

  tmp1 <- dip2
  tmp1$batch <- as.character(tmp1$batch)

  tmp2 <- rbind(dip2[dip2$batch == "b0", ],
               dip2[dip2$batch == "b4" & dip2$tablet %in% as.character(1:6), ])

  # <-><-><-><->

  expect_error(
    f1(data = as.matrix(dip2[dip2$batch %in% c("b0", "b4"), 5:8]),
       tcol = 5:8, grouping = "batch", use_EMA = "no", bounds = c(1, 85)),
    "data must be provided as data frame")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = "tcol", grouping = "batch", use_EMA = "no", bounds = c(1, 85)),
    "tcol must be an integer vector")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:6, grouping = "batch", use_EMA = "no", bounds = c(1, 85)),
    "tcol must be an integer vector")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8 + 0.1, grouping = "batch", use_EMA = "no", bounds = c(1, 85)),
    "tcol must be an integer vector")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:9, grouping = "batch", use_EMA = "no", bounds = c(1, 85)),
    "Some columns specified by tcol")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 3:8, grouping = "batch", use_EMA = "no", bounds = c(1, 85)),
    "Some names of columns specified by tcol")
  expect_error(
    f1(data = tmp0[tmp0$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_EMA = "no", bounds = c(1, 85)),
    "Some columns specified by tcol are not numeric")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = 5, use_EMA = "no", bounds = c(1, 85)),
    "grouping must be string")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "lot", use_EMA = "no", bounds = c(1, 85)),
    "grouping variable was not found")
  expect_error(
    f1(data = tmp1[tmp1$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_EMA = "no", bounds = c(1, 85)),
    "grouping variable's column in data")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b3", "b4"), ],
       tcol = 5:8, grouping = "batch", use_EMA = "no", bounds = c(1, 85)),
    "number of levels in column")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_EMA = "maybe", bounds = c(1, 85)),
    "specify use_EMA either as \"yes\" or \"no\"")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_EMA = 0, bounds = c(1, 85)),
    "specify use_EMA either as \"yes\" or \"no\" or \"ignore\"")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_EMA = "no", bounds = c("1", "85")),
    "bounds must be a numeric vector of length 2")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_EMA = "no", bounds = c(1, 85, 100)),
    "bounds must be a numeric vector of length 2")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_EMA = "no", bounds = c(85, 1)),
    "specify bounds in the form")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_EMA = "no", bounds = c(-1, 85)),
    "specify bounds in the range")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_EMA = "no", bounds = c(1, 101)),
    "specify bounds in the range")
  expect_error(
    f1(data = tmp2,
       tcol = 5:8, grouping = "batch", use_EMA = "yes", bounds = c(1, 85)),
    "The two groups to be compared")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b1"), ],
       tcol = 6:8, grouping = "batch", use_EMA = "yes", bounds = c(1, 85)),
    "According to EMA the profiles")
})

test_that("f1_warns", {
  tcol <- c(5, 6, 7, 8)

  tmp <- rbind(dip2[dip2$batch == "b0", ],
               dip2[dip2$batch == "b4" & dip2$tablet %in% as.character(1:6), ])

  # <-><-><-><->

  expect_warning(
    f1(data = tmp,
       tcol = 5:8, grouping = "batch", use_EMA = "no", bounds = c(1, 85)),
    "The two groups to be compared")
  expect_error(
    expect_warning(
      f1(data = dip2[dip2$batch %in% c("b0", "b1"), ],
         tcol = 6:8, grouping = "batch", use_EMA = "no", bounds = c(1, 85)),
      "According to EMA the two profiles"),
  "The parameter tcol must be an integer")
})

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
