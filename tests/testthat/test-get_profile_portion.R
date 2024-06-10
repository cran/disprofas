context("Profile portion determination")

test_that("get_profile_portion_succeeds", {
  tico_names <- names(get_profile_portion(
    data = dip1, tcol = 3:10, groups = (dip1$type == "R"), use_ema = "yes",
    bounds = c(1, 80), nsf = c(1, 2)))

  res1 <- unname(get_profile_portion(
    data = dip1, tcol = 3:10, groups = (dip1$type == "R"), use_ema = "yes",
    bounds = c(1, 80), nsf = c(1, 2)))

  res2 <- unname(get_profile_portion(
    data = dip1, tcol = 3:10, groups = (dip1$type == "R"), use_ema = "no",
    bounds = c(1, 80), nsf = c(1, 2)))

  res3 <- unname(get_profile_portion(
    data = dip1, tcol = 3:10, groups = (dip1$type == "R"), use_ema = "ignore",
    bounds = c(1, 85), nsf = c(1, 2)))

  res4 <- unname(get_profile_portion(
    data = dip6, tcol = 3:31, groups = (dip6$type == "R"), use_ema = "yes",
    bounds = c(1, 80), nsf = c(1, 2)))

  res5a <- unname(get_profile_portion(
    data = dip6, tcol = 3:31, groups = (dip6$type == "R"), use_ema = "no",
    bounds = c(1, 82), nsf = c(1, 2)))
  res5b <- unname(get_profile_portion(
    data = dip6, tcol = 3:31, groups = (dip6$type == "R"), use_ema = "no",
    bounds = c(1, 82), nsf = c(1, 4)))
  res6a <- unname(get_profile_portion(
    data = dip6, tcol = 3:31, groups = (dip6$type == "R"), use_ema = "no",
    bounds = c(1, 86), nsf = c(1, 2)))
  res6b <- unname(get_profile_portion(
    data = dip6, tcol = 3:31, groups = (dip6$type == "R"), use_ema = "no",
    bounds = c(1, 86), nsf = c(1, 4)))
  res7a <- unname(get_profile_portion(
    data = dip6, tcol = 3:31, groups = (dip6$type == "R"), use_ema = "no",
    bounds = c(1, 90), nsf = c(1, 2)))
  res7b <- unname(get_profile_portion(
    data = dip6, tcol = 3:31, groups = (dip6$type == "R"), use_ema = "no",
    bounds = c(1, 90), nsf = c(1, 4)))

  # <-><-><-><->

  expect_equal(
    tico_names,
    c("t.5", "t.10", "t.15", "t.20", "t.30", "t.60", "t.90", "t.120"))
  expect_equal(res1, c(rep(TRUE, 7), FALSE))
  expect_equal(res2, c(rep(TRUE, 6), FALSE, FALSE))
  expect_equal(res3, c(rep(TRUE, 8)))
  expect_equal(res4, c(rep(FALSE, 8), rep(TRUE, 12), rep(FALSE, 9)))

  expect_equal(res5a, c(rep(FALSE, 2), rep(TRUE, 17), rep(FALSE, 10)))
  expect_equal(res5b, c(rep(FALSE, 2), rep(TRUE, 16), rep(FALSE, 11)))
  expect_equal(res6a, c(rep(FALSE, 2), rep(TRUE, 19), rep(FALSE, 8)))
  expect_equal(res6b, c(rep(FALSE, 2), rep(TRUE, 18), rep(FALSE, 9)))
  expect_equal(res7a, c(rep(FALSE, 2), rep(TRUE, 24), rep(FALSE, 3)))
  expect_equal(res7b, c(rep(FALSE, 2), rep(TRUE, 20), rep(FALSE, 7)))
})

test_that("get_profile_portion_copes_with_NAs", {
  t_dat <- dip1
  t_dat[1, "t.30"] <- NA
  t_dat[2, "t.60"] <- Inf
  t_dat[3, "t.90"] <- NaN

  res1 <- unname(get_profile_portion(
    data = t_dat, tcol = 3:10, groups = (t_dat$type == "R"), use_ema = "yes",
    bounds = c(1, 80), nsf = c(1, 2)))

  res2 <- unname(get_profile_portion(
    data = t_dat, tcol = 3:10, groups = (t_dat$type == "R"), use_ema = "no",
    bounds = c(1, 90), nsf = c(1, 2)))

  # <-><-><-><->

  expect_equal(res1, c(rep(TRUE, 4), rep(FALSE, 4)))
  expect_equal(res2, c(rep(TRUE, 4), rep(FALSE, 4)))
})

test_that("get_profile_portion_fails", {
  tmp0 <- dip1
  tmp0$t.5 <- as.factor(tmp0$t.5)

  # <-><-><-><->

  expect_error(
    get_profile_portion(
      data = as.matrix(dip1[, 3:10]), tcol = 3:10, groups = (dip1$type == "R"),
      use_ema = "yes", bounds = c(1, 80), nsf = c(1, 2)),
    "data must be provided as data frame")
  expect_error(
    get_profile_portion(data = dip1, tcol = "tcol", groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c(1, 80), nsf = c(1, 2)),
    "tcol must be an integer vector")
  expect_error(
    get_profile_portion(data = dip1, tcol = 1, groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c(1, 80), nsf = c(1, 2)),
    "tcol must be an integer vector")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10 + 0.1,
                        groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c(1, 80), nsf = c(1, 2)),
    "tcol must be an integer vector")
  expect_error(
    get_profile_portion(data = dip1, tcol = 7:11, groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c(1, 80), nsf = c(1, 2)),
    "Some columns specified by tcol")
  expect_error(
    get_profile_portion(data = dip1, tcol = 2:6, groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c(1, 80), nsf = c(1, 2)),
    "Some names of columns specified by tcol")
  expect_error(
    get_profile_portion(data = tmp0, tcol = 3:10, groups = (tmp0$type == "R"),
                        use_ema = "yes", bounds = c(1, 80), nsf = c(1, 2)),
    "Some columns specified by tcol are not numeric")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = dip1$type,
                        use_ema = "yes", bounds = c(1, 80), nsf = c(1, 2)),
    "groups must be a logical vector")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = rep(TRUE, 6),
                        use_ema = "yes", bounds = c(1, 80), nsf = c(1, 2)),
    "groups must be a logical vector")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_ema = "maybe", bounds = c(1, 80), nsf = c(1, 2)),
    "specify use_ema either as \"yes\" or \"no\" or \"ignore\"")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c("1", "80"), nsf = c(1, 2)),
    "bounds must be a numeric vector of length 2")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c(1, 80, 100), nsf = c(1, 2)),
    "bounds must be a numeric vector of length 2")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c(80, 1), nsf = c(1, 2)),
    "specify bounds in the form")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c(-1, 80), nsf = c(1, 2)),
    "specify bounds in the range")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c(1, 101), nsf = c(1, 2)),
    "specify bounds in the range")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c(1, 85), nsf = c("1", "2")),
    "nsf must be a positive integer")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c(1, 85), nsf = c(-1, 2)),
    "nsf must be a positive integer")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c(1, 85), nsf = c(1, -2)),
    "nsf must be a positive integer")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c(1, 85), nsf = 4),
    "nsf must be a positive integer")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_ema = "yes", bounds = c(1, 85), nsf = c(4.4, 3.3)),
    "nsf must be a positive integer")
})
