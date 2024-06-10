context("Get Hotelling's T2 statistics for one (small) sample")

test_that("get_T2_one_succeeds", {
  # Test with data set from Tsong (1996)
  l_res1 <- get_T2_one(m = as.matrix(dip1[dip1$type == "R", c(5, 9)]),
                       mu = round(colMeans(
                         as.matrix(dip1[dip1$type == "T", c(5, 9)])), 0),
                       signif = 0.1, na_rm = FALSE)

  # Test with data set from Tsong (1997)
  l_res2 <- get_T2_one(m = as.matrix(dip7[dip7$type == "ref", 4:5]),
                       mu = colMeans(as.matrix(dip7[dip7$type == "test", 4:5])),
                       signif = 0.05, na_rm = FALSE)

  # Test with data set from Sathe (1996)
  l_res3_min <-
    get_T2_one(m = log(as.matrix(dip8[dip8$type == "ref", 3:4])),
               mu = log(colMeans(as.matrix(dip8[dip8$type == "minor", 3:4]))),
               signif = 0.1, na_rm = FALSE)

  l_res3_maj <-
    get_T2_one(m = log(as.matrix(dip8[dip8$type == "ref", 3:4])),
               mu = log(colMeans(as.matrix(dip8[dip8$type == "major", 3:4]))),
               signif = 0.1, na_rm = FALSE)

  # <-><-><-><->

  expect_equal(signif(l_res1$Parameters[["dm"]], 7), 9.729112)
  expect_equal(round(l_res1$Parameters[["df1"]], 0), 2)
  expect_equal(round(l_res1$Parameters[["df2"]], 0), 4)
  expect_equal(signif(l_res1$Parameters[["K"]], 7), 2.400000)
  expect_equal(round(l_res1$Parameters[["k"]], 0), 6)
  expect_equal(signif(l_res1$Parameters[["T2"]], 7), 567.9337)
  expect_equal(signif(l_res1$Parameters[["F"]], 7), 227.1735)
  expect_equal(signif(l_res1$Parameters[["F.crit"]], 7), 4.324555)
  expect_equal(signif(l_res1$Parameters[["p.F"]], 7), 7.616076e-05)

  expect_equal(signif(l_res2$Parameters[["dm"]], 7), 3.027907)
  expect_equal(round(l_res2$Parameters[["df1"]], 0), 2)
  expect_equal(round(l_res2$Parameters[["df2"]], 0), 34)
  expect_equal(signif(l_res2$Parameters[["K"]], 7), 17.48571)
  expect_equal(round(l_res2$Parameters[["k"]], 0), 36)
  expect_equal(signif(l_res2$Parameters[["T2"]], 7), 330.0560)
  expect_equal(signif(l_res2$Parameters[["F"]], 7), 160.3129)
  expect_equal(signif(l_res2$Parameters[["F.crit"]], 7), 3.275898)
  expect_equal(signif(l_res2$Parameters[["p.F"]], 7), 0.0)

  expect_equal(signif(l_res3_min$Parameters[["dm"]], 7), 1.295649)
  expect_equal(round(l_res3_min$Parameters[["df1"]], 0), 2)
  expect_equal(round(l_res3_min$Parameters[["df2"]], 0), 10)
  expect_equal(signif(l_res3_min$Parameters[["K"]], 7), 5.454545)
  expect_equal(round(l_res3_min$Parameters[["k"]], 0), 12)
  expect_equal(signif(l_res3_min$Parameters[["T2"]], 7), 20.14446)
  expect_equal(signif(l_res3_min$Parameters[["F"]], 7), 9.156574)
  expect_equal(signif(l_res3_min$Parameters[["F.crit"]], 7), 2.924466)
  expect_equal(signif(l_res3_min$Parameters[["p.F"]], 7), 0.005496158)

  expect_equal(signif(l_res3_maj$Parameters[["dm"]], 7), 4.325388)
  expect_equal(round(l_res3_maj$Parameters[["df1"]], 0), 2)
  expect_equal(round(l_res3_maj$Parameters[["df2"]], 0), 10)
  expect_equal(signif(l_res3_maj$Parameters[["K"]], 7), 5.454545)
  expect_equal(round(l_res3_maj$Parameters[["k"]], 0), 12)
  expect_equal(signif(l_res3_maj$Parameters[["T2"]], 7), 224.5078)
  expect_equal(signif(l_res3_maj$Parameters[["F"]], 7), 102.0490)
  expect_equal(signif(l_res3_maj$Parameters[["F.crit"]], 7), 2.924466)
  expect_equal(signif(l_res3_maj$Parameters[["p.F"]], 7), 2.222987e-07)
})

test_that("get_T2_one_successfully_calculates_CIs", {
  l_res <-
    get_T2_one(m = as.matrix(dip12), mu = c(1000, 15, 60, 800, 75),
                          signif = 0.05, na_rm = FALSE)

  # <-><-><-><->

  expect_equal(round(l_res[["Parameters"]][["df1"]], 0), 5)
  expect_equal(round(l_res[["Parameters"]][["df2"]], 0), 732)
  expect_equal(signif(l_res[["Parameters"]][["T2"]], 7), 1758.541)
  expect_equal(signif(l_res[["Parameters"]][["F"]], 7), 349.7968)
  expect_equal(signif(l_res[["Parameters"]][["p.F"]], 7), 0.0000000)
  expect_equal(signif(l_res[["Parameters"]][["F.crit"]], 7), 2.226340)
  expect_equal(signif(l_res[["Parameters"]][["t.crit"]], 7), 2.582526)

  # <-><-><-><->

  expect_equal(signif(l_res[["CI"]][["Hotelling"]][, "LCL"], 7),
               c(575.0912, 10.39244, 62.03547, 638.3278, 69.85901))
  expect_equal(signif(l_res[["CI"]][["Hotelling"]][, "UCL"], 7),
               c(673.0073, 11.86735, 69.57141, 1040.943, 87.99788))

  # <-><-><-><->

  expect_equal(signif(l_res[["CI"]][["Bonferroni"]][, "LCL"], 7),
               c(586.2568, 10.56063, 62.89481, 684.2391, 71.92743))
  expect_equal(signif(l_res[["CI"]][["Bonferroni"]][, "UCL"], 7),
               c(661.8417, 11.69917, 68.71207, 995.0316, 85.92946))
})

test_that("get_T2_one_copes_with_NAs", {
  m_ref <- as.matrix(dip7[dip7$type == "ref", 4:5])
  m_ref[1, "alpha"] <- NA
  m_ref[25, "alpha"] <- NA
  m_ref[13, "beta"] <- NaN

  l_res <- get_T2_one(m = m_ref, mu = c(0.4, 0.7), signif = 0.05, na_rm = TRUE)

  # <-><-><-><->

  expect_equal(signif(l_res$Parameters[["dm"]], 7), 2.743059)
  expect_equal(round(l_res$Parameters[["df1"]], 0), 2)
  expect_equal(round(l_res$Parameters[["df2"]], 0), 31)
  expect_equal(signif(l_res$Parameters[["K"]], 7), 15.98438)
  expect_equal(round(l_res$Parameters[["k"]], 0), 33)
  expect_equal(signif(l_res$Parameters[["T2"]], 7), 248.3043)
  expect_equal(signif(l_res$Parameters[["F"]], 7), 120.2724)
  expect_equal(signif(l_res$Parameters[["F.crit"]], 7), 3.304817)
  expect_equal(signif(l_res$Parameters[["p.F"]], 7), 0.0)

  expect_equal(signif(l_res[["CI"]][["Hotelling"]][, "LCL"], 7),
               c(0.5213030, 0.4822436))
  expect_equal(signif(l_res[["CI"]][["Hotelling"]][, "UCL"], 7),
               c(0.5732958, 0.5506085))
  expect_equal(signif(l_res[["CI"]][["Bonferroni"]][, "LCL"], 7),
               c(0.5238928, 0.4856490))
  expect_equal(signif(l_res[["CI"]][["Bonferroni"]][, "UCL"], 7),
               c(0.5707059, 0.5472031))
})

test_that("get_T2_one_sends_message", {
  m_ref <- as.matrix(dip7[dip7$type == "ref", 4:5])
  m_ref[1, "alpha"] <- NA
  m_ref[25, "alpha"] <- NA
  m_ref[13, "beta"] <- NaN

  # <-><-><-><->

  expect_message(get_T2_one(m = m_ref, mu = c(0.4, 0.7), signif = 0.05,
                            na_rm = FALSE),
                 "m contains NA/NaN values")
})

test_that("get_T2_one_fails", {
  expect_error(
    get_T2_one(m = dip12, mu = c(1000, 15, 60, 800, 75),
                          signif = 0.05, na_rm = TRUE),
    "m must be a matrix")
  expect_error(
    get_T2_one(m = as.matrix(dip12),
                          mu = as.character(c(1000, 15, 60, 800, 75)),
                          signif = 0.05, na_rm = TRUE),
    "mu must be a numeric vector")
  expect_error(
    get_T2_one(m = as.matrix(dip12), mu = c(1000, 15, 60, 800),
                          signif = 0.05, na_rm = TRUE),
    "number of columns in m")
  expect_error(
    get_T2_one(m = as.matrix(dip12), mu = c(1000, NA, 60, 800, 75),
               signif = 0.05, na_rm = TRUE),
    "mu contains NA/NaN/Inf values")
  expect_error(
    get_T2_one(m = as.matrix(dip12), mu = c(1000, 15, 60, 800, 75),
                          signif = -1, na_rm = TRUE),
    "specify signif")
  expect_error(
    get_T2_one(m = as.matrix(dip12), mu = c(1000, 15, 60, 800, 75),
                          signif = 9, na_rm = TRUE),
    "specify signif")
  expect_error(
    get_T2_one(m = as.matrix(dip12), mu = c(1000, 15, 60, 800, 75),
               signif = 0.05, na_rm = 1),
    "na_rm must be a logical")
  expect_error(
    get_T2_one(m = as.matrix(dip12), mu = c(1000, 15, 60, 800, 75),
               signif = 0.05, na_rm = c(TRUE, FALSE)),
    "na_rm must be a logical")
})
