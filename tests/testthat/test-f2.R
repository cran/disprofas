context("f2")

test_that("f2_results_match", {
  t_dat <- dip6
  t_dat[1, "t.40"] <- NA
  t_dat[12, "t.45"] <- NA
  t_dat[13, "t.90"] <- NaN
  t_dat[24, "t.95"] <- NaN

  # <-><-><-><->

  l_res1 <- f2(data = dip2[dip2$batch %in% c("b0", "b1"), ],
               tcol = 5:8, grouping = "batch", use_ema = "no",
               bounds = c(1, 100), nsf = c(1, 3))
  l_res2 <- f2(data = dip2[dip2$batch %in% c("b0", "b2"), ],
               tcol = 5:8, grouping = "batch", use_ema = "no",
               bounds = c(1, 100), nsf = c(1, 3))
  l_res3 <- f2(data = dip2[dip2$batch %in% c("b0", "b3"), ],
               tcol = 5:8, grouping = "batch", use_ema = "no",
               bounds = c(1, 100), nsf = c(1, 3))
  l_res4 <- f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
               tcol = 5:8, grouping = "batch", use_ema = "no",
               bounds = c(1, 100), nsf = c(1, 3))
  l_res5 <- f2(data = dip2[dip2$batch %in% c("b0", "b5"), ],
               tcol = 5:8, grouping = "batch", use_ema = "no",
               bounds = c(1, 100), nsf = c(1, 3))

  # No message is sent because the columns drop out when the profile
  # portion is determined by aid of the function get_profile_portion()
  l_res6 <- f2(data = t_dat, tcol = 3:31, grouping = "type",
               use_ema = "yes", bounds = c(1, 85), nsf = c(1, 2))

  # <-><-><-><->

  expect_equal(round(l_res1$f2, 2), 60.03)
  expect_equal(round(l_res2$f2, 2), 51.08)
  expect_equal(round(l_res3$f2, 2), 51.19)
  expect_equal(round(l_res4$f2, 2), 50.07)
  expect_equal(round(l_res5$f2, 2), 48.05)
  expect_equal(round(l_res6$f2, 2), 92.99)
})

test_that("f2_sends_message", {
  t_dat <- dip2[dip2$batch %in% c("b0", "b1"), ]
  t_dat[1, "t.30"] <- NA
  t_dat[12, "t.60"] <- NA
  t_dat[13, "t.90"] <- NaN
  t_dat[24, "t.180"] <- NaN

  # <-><-><-><->

  res <- expect_message(f2(data = t_dat, tcol = 5:8, grouping = "batch",
                           use_ema = "ignore", bounds = c(1, 100)),
                        nsf = c(1, 3))
  expect_equal(res[["f2"]], NA_real_)
})

test_that("f2_warns", {
  tcol <- c(5, 6, 7, 8)

  tmp <- rbind(dip2[dip2$batch == "b0", ],
               dip2[dip2$batch == "b4" & dip2$tablet %in% as.character(1:6), ])

  # <-><-><-><->

  expect_warning(
    f2(data = tmp,
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = c(1, 2)),
    "The two groups to be compared")
  expect_error(
    expect_warning(
      f2(data = dip2[dip2$batch %in% c("b0", "b1"), ],
         tcol = 6:8, grouping = "batch", use_ema = "no", bounds = c(1, 85),
         nsf = c(1, 2)),
      "According to EMA the two profiles"),
    "The parameter tcol must be an integer")
})

test_that("f2_fails", {
  tmp0 <- dip2
  tmp0$t.30 <- as.factor(tmp0$t.30)

  tmp1 <- dip2
  tmp1$batch <- as.character(tmp1$batch)

  tmp2 <- rbind(dip2[dip2$batch == "b0", ],
                dip2[dip2$batch == "b4" & dip2$tablet %in% as.character(1:6), ])

  # <-><-><-><->

  expect_error(
    f2(data = as.matrix(dip2[dip2$batch %in% c("b0", "b4"), 5:8]),
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = c(1, 2)),
    "data must be provided as data frame")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = "tcol", grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = c(1, 2)),
    "tcol must be an integer vector")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:6, grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = c(1, 2)),
    "tcol must be an integer vector")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8 + 0.1, grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = c(1, 2)),
    "tcol must be an integer vector")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:9, grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = c(1, 2)),
    "Some columns specified by tcol were not found")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 3:8, grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = c(1, 2)),
    "Some names of columns specified by tcol")
  expect_error(
    f2(data = tmp0[tmp0$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = c(1, 2)),
    "Some columns specified by tcol are not numeric")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = 5, use_ema = "no", bounds = c(1, 85),
       nsf = c(1, 2)),
    "grouping must be string")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "lot", use_ema = "no", bounds = c(1, 85),
       nsf = c(1, 2)),
    "grouping variable was not found")
  expect_error(
    f2(data = tmp1[tmp1$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = c(1, 2)),
    "grouping variable's column in data")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b3", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = c(1, 2)),
    "number of levels in column")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = "maybe", bounds = c(1, 85),
       nsf = c(1, 2)),
    "specify use_ema either as \"yes\" or \"no\"")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = 0, bounds = c(1, 85),
       nsf = c(1, 2)),
    "specify use_ema either as \"yes\" or \"no\" or \"ignore\"")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c("1", "85"),
       nsf = c(1, 2)),
    "bounds must be a numeric vector of length 2")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c(1, 85, 100),
       nsf = c(1, 2)),
    "bounds must be a numeric vector of length 2")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c(85, 1),
       nsf = c(1, 2)),
    "specify bounds in the form")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c(-1, 85),
       nsf = c(1, 2)),
    "specify bounds in the range")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c(1, 101),
       nsf = c(1, 2)),
    "specify bounds in the range")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = c("1", "2")),
    "nsf must be a positive integer")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = c(-1, 2)),
    "nsf must be a positive integer")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = c(1, -2)),
    "nsf must be a positive integer")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = 4),
    "nsf must be a positive integer")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:8, grouping = "batch", use_ema = "no", bounds = c(1, 85),
       nsf = c(4.4, 3.3)),
    "nsf must be a positive integer")

  expect_error(
    f2(data = tmp2,
       tcol = 5:8, grouping = "batch", use_ema = "yes", bounds = c(1, 85),
       nsf = c(1, 2)),
    "The two groups to be compared")
  expect_error(
    f2(data = dip2[dip2$batch %in% c("b0", "b1"), ],
       tcol = 6:8, grouping = "batch", use_ema = "yes", bounds = c(1, 85),
       nsf = c(1, 2)),
    "According to EMA the profiles")
})
