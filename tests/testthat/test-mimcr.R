context("Model-Independent Multivariate Confidence Region Estimation")

test_that("mimcr_results_match_Hoffelder_2015", {
  l_res <-
    mimcr(data = dip3, tcol = 4:6, grouping = "batch", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, bounds = c(1, 85),
          tol = 1e-9)

  # <-><-><-><->

  expect_equal(signif(l_res$Parameters[["DM"]], 7), 0.2384023)
  expect_equal(round(l_res$Parameters[["df1"]], 0), 3)
  expect_equal(round(l_res$Parameters[["df2"]], 0), 20)
  expect_equal(round(l_res$Parameters[["alpha"]], 2), 0.05)
  expect_equal(signif(l_res$Parameters[["K"]], 7), 1.818182)
  expect_equal(round(l_res$Parameters[["k"]], 0), 6)
  expect_equal(signif(l_res$Parameters[["T2"]], 7), 0.3410141)
  expect_equal(signif(l_res$Parameters[["F"]], 7), 0.1033376)
  expect_equal(signif(l_res$Parameters[["ncp.Hoffelder"]], 7), 30.32296)
  expect_equal(signif(l_res$Parameters[["F.crit"]], 7), 3.098391)
  expect_equal(signif(l_res$Parameters[["F.crit.Hoffelder"]], 7), 4.899274)
  expect_equal(signif(l_res$Parameters[["p.F"]], 7), 0.9571526)
  expect_equal(signif(l_res$Parameters[["p.F.Hoffelder"]], 7), 2.890827e-08)
  expect_equal(signif(l_res$Parameters[["Sim.Limit"]], 7), 2.248072)
  expect_equal(signif(l_res$Parameters[["Obs.L"]], 7), 1.067015)
  expect_equal(signif(l_res$Parameters[["Obs.U"]], 7), 1.543820)
})

test_that("mimcr_results_match_Hoffelder_2016", {
  l_res <- mimcr(data = dip4, tcol = 2:4, grouping = "type", fit_n_obs = FALSE,
                 mtad = 10, signif = 0.05, max_trial = 50, bounds = c(1, 85),
                 tol = 1e-9)

  # <-><-><-><->

  expect_equal(signif(l_res$Parameters[["DM"]], 7), 2.823976)
  expect_equal(round(l_res$Parameters[["df1"]], 0), 3)
  expect_equal(round(l_res$Parameters[["df2"]], 0), 20)
  expect_equal(round(l_res$Parameters[["alpha"]], 2), 0.05)
  expect_equal(signif(l_res$Parameters[["K"]], 7), 1.818182)
  expect_equal(round(l_res$Parameters[["k"]], 0), 6)
  expect_equal(signif(l_res$Parameters[["T2"]], 7), 47.84903)
  expect_equal(signif(l_res$Parameters[["F"]], 7), 14.49970)
  expect_equal(signif(l_res$Parameters[["ncp.Hoffelder"]], 7), 1770.045)
  expect_equal(signif(l_res$Parameters[["F.crit"]], 7), 3.098391)
  expect_equal(signif(l_res$Parameters[["F.crit.Hoffelder"]], 7), 373.4880)
  expect_equal(signif(l_res$Parameters[["p.F"]], 7), 0.00003002744)
  expect_equal(signif(l_res$Parameters[["p.F.Hoffelder"]], 7), 8.427879e-110)
  expect_equal(signif(l_res$Parameters[["Sim.Limit"]], 7), 17.17578)
  expect_equal(signif(l_res$Parameters[["Obs.L"]], 7), 1.518558)
  expect_equal(signif(l_res$Parameters[["Obs.U"]], 7), 4.129393)
})

test_that("mimcr_results_match_Tsong_1996_two", {
  l_res <-
    suppressWarnings(mimcr(data = dip1[, c("type", "tablet", "t.15", "t.90")],
                           tcol = 3:4, grouping = "type", fit_n_obs = FALSE,
                           mtad = 15, signif = 0.1, max_trial = 50,
                           bounds = c(1, 100), tol = 1e-9))

  # <-><-><-><->

  expect_equal(signif(l_res$Parameters[["DM"]], 7), 10.44045)
  expect_equal(round(l_res$Parameters[["df1"]], 0), 2)
  expect_equal(round(l_res$Parameters[["df2"]], 0), 9)
  expect_equal(round(l_res$Parameters[["alpha"]], 2), 0.1)
  expect_equal(signif(l_res$Parameters[["K"]], 7), 1.350000)
  expect_equal(signif(l_res$Parameters[["T2"]], 7), 327.0089)
  expect_equal(signif(l_res$Parameters[["F.crit"]], 7), 3.006452)
  expect_equal(signif(l_res$Parameters[["Sim.Limit"]], 7), 9.630777)
  expect_equal(signif(l_res$Parameters[["Obs.L"]], 7), 8.948135)
  expect_equal(signif(l_res$Parameters[["Obs.U"]], 7), 11.93276)

  expect_equivalent(signif(l_res[["NR.CI"]][["CI"]][, 1], 7),
                    c(-15.03433, 2.902591))
  expect_equivalent(signif(l_res[["NR.CI"]][["CI"]][, 2], 7),
                    c(-20.04900, 3.870743))
})

test_that("mimcr_results_match_Tsong_1996_all", {
  l_res <-
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.1, bounds = c(1, 100), tol = 1e-9)

  # <-><-><-><->

  expect_equal(signif(l_res$Parameters[["DM"]], 7), 26.48562)
  expect_equal(round(l_res$Parameters[["df1"]], 0), 8)
  expect_equal(round(l_res$Parameters[["df2"]], 0), 3)
  expect_equal(round(l_res$Parameters[["alpha"]], 2), 0.1)
  expect_equal(signif(l_res$Parameters[["K"]], 7), 0.1125000)
  expect_equal(signif(l_res$Parameters[["T2"]], 7), 2104.464)
  expect_equal(signif(l_res$Parameters[["F.crit"]], 7), 5.251671)
  expect_equal(signif(l_res$Parameters[["Sim.Limit"]], 7), 19.42719)
  expect_equal(signif(l_res$Parameters[["Obs.L"]], 7), 19.65323)
  expect_equal(signif(l_res$Parameters[["Obs.U"]], 7), 33.31800)

  expect_equivalent(signif(l_res[["NR.CI"]][["CI"]][, 1], 7),
                    c(-17.58002, -15.22654, -13.01651, -10.97963, -7.432708,
                      -0.5169504, 2.513022, 3.708933))
  expect_equivalent(signif(l_res[["NR.CI"]][["CI"]][, 2], 7),
                    c(-29.80331, -25.81346, -22.06682, -18.61370, -12.60063,
                       -0.8763830, 4.260311, 6.287733))
})

test_that("mimcr_fails", {
  tmp0 <- dip1
  tmp0$t.5 <- as.factor(tmp0$t.5)

  tmp1 <- dip1
  tmp1$type <-  as.character(tmp1$type)

  tmp2 <- dip1[1:11, ]

  # <-><-><->

  expect_error(
    mimcr(data = as.matrix(dip1[, 3:10]), tcol = 3:10, grouping = "type",
          fit_n_obs = FALSE, mtad = 10, signif = 0.05, max_trial = 50,
          bounds = c(1, 85), tol = 1e-9),
    "data must be provided as data frame")
  expect_error(
    mimcr(data = dip1, tcol = "tico", grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50,
          bounds = c(1, 85), tol = 1e-9),
    "tcol must be an integer")
  expect_error(
    suppressWarnings(mimcr(data = dip1, tcol = 3, grouping = "type",
                           fit_n_obs = FALSE, mtad = 10, signif = 0.05,
                           max_trial = 50, bounds = c(1, 85), tol = 1e-9)),
    "tcol must be an integer")
  expect_error(
    mimcr(data = dip1, tcol = 3:10 + 0.1, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50,  bounds = c(1, 85),
          tol = 1e-9),
    "tcol must be an integer")
  expect_error(
    mimcr(data = dip1, tcol = 7:11, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50,  bounds = c(1, 85),
          tol = 1e-9),
    "Some columns specified by tcol were not found")
  expect_error(
    mimcr(data = dip1, tcol = 2:6, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50,  bounds = c(1, 85),
          tol = 1e-9),
    "Some names of columns specified by tcol")
  expect_error(
    mimcr(data = tmp0, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50,  bounds = c(1, 85),
          tol = 1e-9),
    "Some columns specified by tcol are not numeric")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = 5, fit_n_obs = FALSE, mtad = 10,
          signif = 0.05, max_trial = 50, bounds = c(1, 85), tol = 1e-9),
    "grouping must be string")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "lot", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, bounds = c(1, 85),
          tol = 1e-9),
    "grouping variable was not found")
  expect_error(
    mimcr(data = tmp1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, bounds = c(1, 85),
          tol = 1e-9),
    "grouping variable's column in data")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "tablet", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, bounds = c(1, 85),
          tol = 1e-9),
    "number of levels in column")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = "FALSE",
          mtad = 10, signif = 0.05, max_trial = 50, bounds = c(1, 85),
          tol = 1e-9),
    "fit_n_obs must be a logical")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = c(T, F),
          mtad = 10, signif = 0.05, max_trial = 50, bounds = c(1, 85),
          tol = 1e-9),
    "fit_n_obs must be a logical")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = -5, signif = 0.05, max_trial = 50, bounds = c(1, 85),
          tol = 1e-9),
    "specify mtad")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 55, signif = 0.05, max_trial = 50, bounds = c(1, 85),
          tol = 1e-9),
    "specify mtad")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = -1, max_trial = 50, bounds = c(1, 85),
          tol = 1e-9),
    "specify signif")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 9, max_trial = 50, bounds = c(1, 85),
          tol = 1e-9),
    "specify signif")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = "max_trial", bounds = c(1, 85),
          tol = 1e-9),
    "max_trial must be a positive integer")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = c(50, 100), bounds = c(1, 85),
          tol = 1e-9),
    "max_trial must be a positive integer")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50.5, bounds = c(1, 85),
          tol = 1e-9),
    "max_trial must be a positive integer")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = -10, bounds = c(1, 85),
          tol = 1e-9),
    "max_trial must be a positive integer")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, bounds = c("1", "85"),
          tol = 1e-9),
    "bounds must be a numeric vector of length 2")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, bounds = c(1, 85, 100),
          tol = 1e-9),
    "bounds must be a numeric vector of length 2")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, bounds = c(85, 1),
          tol = 1e-9),
    "specify bounds in the form")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, bounds = c(-1, 85),
          tol = 1e-9),
    "specify bounds in the range")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, bounds = c(1, 101),
          tol = 1e-9),
    "specify bounds in the range")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50,  bounds = c(1, 85),
          tol = "1e-9"),
    "tol must be a non-negative numeric")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50,  bounds = c(1, 85),
          tol = rep(1e-9, 2)),
    "tol must be a non-negative numeric")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50,  bounds = c(1, 85),
          tol = -1e-9),
    "tol must be a non-negative numeric")
  expect_error(
    mimcr(data = tmp2, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50,  bounds = c(1, 85),
          tol = 1e-9),
    "The treatments to be tested")
})

test_that("mimcr_warns", {
  tmp <- rbind(dip2[dip2$batch == "b0", ],
               dip2[dip2$batch == "b4" & dip2$tablet %in% as.character(1:6), ])

  # <-><-><-><->

  expect_warning(
    mimcr(data = tmp, tcol = 4:8, grouping = "type", fit_n_obs = TRUE,
          mtad = 10, signif = 0.1, max_trial = 50,  bounds = c(1, 85),
          tol = 1e-9),
    "Rows from the group with redundant observations")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  expect_warning(
    mimcr(data = dip3, tcol = 4:6, grouping = "type", fit_n_obs = TRUE,
          mtad = 10, signif = 0.1, max_trial = 50,  bounds = c(1, 55),
          tol = 1e-9),
    "The profiles should comprise a minimum of 3 time points")
  expect_warning(
    mimcr(data = dip3, tcol = 4:6, grouping = "type", fit_n_obs = TRUE,
          mtad = 10, signif = 0.1, max_trial = 50,  bounds = c(1, 85),
          tol = 1),
    "The points found by the Newton-Raphson search")
})

test_that("mimcr_nera_estimation_fails", {
  tmp1 <- expect_warning(
    mimcr(data = dip3, tcol = 4:6, grouping = "type", fit_n_obs = TRUE,
          mtad = 10, signif = 0.1, max_trial = 5,  bounds = c(1, 85),
          tol = 1e-15),
    "Newton-Raphson search did not converge")

  expect_equivalent(tmp1[["Similarity"]]["Tsong"], "Dissimilar")
  expect_equivalent(tmp1[["NR.CI"]][["CI"]][, "LCL"], rep(NA, 3))
  expect_equivalent(tmp1[["NR.CI"]][["CI"]][, "UCL"], rep(NA, 3))
})
