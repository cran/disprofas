context("Profile portion determination")

test_that("get_profile_portion_succeeds", {
  tico_names <- names(get_profile_portion(
    data = dip1, tcol = 3:10, groups = (dip1$type == "R"), use_EMA = "yes",
    bounds = c(1, 80)))

  res1 <- unname(get_profile_portion(
    data = dip1, tcol = 3:10, groups = (dip1$type == "R"), use_EMA = "yes",
    bounds = c(1, 80)))

  res2 <- unname(get_profile_portion(
    data = dip1, tcol = 3:10, groups = (dip1$type == "R"), use_EMA = "no",
    bounds = c(1, 80)))

  res3 <- unname(get_profile_portion(
    data = dip1, tcol = 3:10, groups = (dip1$type == "R"), use_EMA = "ignore",
    bounds = c(1, 85)))

  expect_equal(
    tico_names,
    c("t.5", "t.10", "t.15", "t.20", "t.30", "t.60", "t.90", "t.120"))
  expect_equal(res1, c(rep(TRUE, 7), FALSE))
  expect_equal(res2, c(rep(TRUE, 6), FALSE, FALSE))
  expect_equal(res3, c(rep(TRUE, 8)))
})

test_that("get_profile_portion_fails", {
  tmp0 <- dip1
  tmp0$t.5 <- as.factor(tmp0$t.5)

  # <-><-><-><->

  expect_error(
    get_profile_portion(
      data = as.matrix(dip1[, 3:10]), tcol = 3:10, groups = (dip1$type == "R"),
      use_EMA = "yes", bounds = c(1, 80)),
    "data must be provided as data frame")
  expect_error(
    get_profile_portion(data = dip1, tcol = "tcol", groups = (dip1$type == "R"),
                        use_EMA = "yes", bounds = c(1, 80)),
    "tcol must be an integer vector")
  expect_error(
    get_profile_portion(data = dip1, tcol = 1, groups = (dip1$type == "R"),
                        use_EMA = "yes", bounds = c(1, 80)),
    "tcol must be an integer vector")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10 + 0.1,
                        groups = (dip1$type == "R"),
                        use_EMA = "yes", bounds = c(1, 80)),
    "tcol must be an integer vector")
  expect_error(
    get_profile_portion(data = dip1, tcol = 7:11, groups = (dip1$type == "R"),
                        use_EMA = "yes", bounds = c(1, 80)),
    "Some columns specified by tcol")
  expect_error(
    get_profile_portion(data = dip1, tcol = 2:6, groups = (dip1$type == "R"),
                        use_EMA = "yes", bounds = c(1, 80)),
    "Some names of columns specified by tcol")
  expect_error(
    get_profile_portion(data = tmp0, tcol = 3:10, groups = (tmp0$type == "R"),
                        use_EMA = "yes", bounds = c(1, 80)),
    "Some columns specified by tcol are not numeric")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = dip1$type,
                        use_EMA = "yes", bounds = c(1, 80)),
    "groups must be a logical vector")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = rep(TRUE, 6),
                        use_EMA = "yes", bounds = c(1, 80)),
    "groups must be a logical vector")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_EMA = "maybe", bounds = c(1, 80)),
    "specify use_EMA either as \"yes\" or \"no\" or \"ignore\"")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_EMA = "yes", bounds = c("1", "80")),
    "bounds must be a numeric vector of length 2")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_EMA = "yes", bounds = c(1, 80, 100)),
    "bounds must be a numeric vector of length 2")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_EMA = "yes", bounds = c(80, 1)),
    "specify bounds in the form")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_EMA = "yes", bounds = c(-1, 80)),
    "specify bounds in the range")
  expect_error(
    get_profile_portion(data = dip1, tcol = 3:10, groups = (dip1$type == "R"),
                        use_EMA = "yes", bounds = c(1, 101)),
    "specify bounds in the range")
})
