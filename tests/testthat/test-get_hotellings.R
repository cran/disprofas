context("Get Hotelling's T2 statistics for two independent (small) samples")

test_that("get_hotellings_succeeds", {
  # Test with data set from Tsong (1996)
  l_res1 <- suppressWarnings(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                   m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                   signif = 0.1))

  # Test with data set from Tsong (1997)
  l_res2 <- suppressWarnings(
    get_hotellings(m1 = as.matrix(dip7[dip7$type == "ref", 4:5]),
                   m2 = as.matrix(dip7[dip7$type == "test", 4:5]),
                   signif = 0.05))

  # Test with data set from Sathe (1996)
  l_res3_min <- suppressWarnings(
    get_hotellings(m1 = log(as.matrix(dip8[dip8$type == "ref", 3:4])),
                   m2 = log(as.matrix(dip8[dip8$type == "minor", 3:4])),
                   signif = 0.1))

  l_res3_maj <- suppressWarnings(
    get_hotellings(m1 = log(as.matrix(dip8[dip8$type == "ref", 3:4])),
                   m2 = log(as.matrix(dip8[dip8$type == "major", 3:4])),
                   signif = 0.1))

  # <-><-><-><->

  expect_equal(signif(l_res1$Parameters[["dm"]], 7), 26.48562)
  expect_equal(round(l_res1$Parameters[["df1"]], 0), 8)
  expect_equal(round(l_res1$Parameters[["df2"]], 0), 3)
  expect_equal(signif(l_res1$Parameters[["K"]], 7), 0.1125000)
  expect_equal(round(l_res1$Parameters[["k"]], 0), 3)
  expect_equal(signif(l_res1$Parameters[["T2"]], 7), 2104.464)
  expect_equal(signif(l_res1$Parameters[["F"]], 7), 78.91739)
  expect_equal(signif(l_res1$Parameters[["F.crit"]], 7), 5.251671)
  expect_equal(signif(l_res1$Parameters[["p.F"]], 7), 0.002116258)

  expect_equal(signif(l_res2$Parameters[["dm"]], 7), 3.247275)
  expect_equal(round(l_res2$Parameters[["df1"]], 0), 2)
  expect_equal(round(l_res2$Parameters[["df2"]], 0), 45)
  expect_equal(signif(l_res2$Parameters[["K"]], 7), 4.402174)
  expect_equal(round(l_res2$Parameters[["k"]], 0), 9)
  expect_equal(signif(l_res2$Parameters[["T2"]], 7), 94.90313)
  expect_equal(signif(l_res2$Parameters[["F"]], 7), 46.42001)
  expect_equal(signif(l_res2$Parameters[["F.crit"]], 7), 3.204317)
  expect_equal(signif(l_res2$Parameters[["p.F"]], 7), 1.151701e-11)

  expect_equal(signif(l_res3_min$Parameters[["dm"]], 7), 1.462604)
  expect_equal(round(l_res3_min$Parameters[["df1"]], 0), 2)
  expect_equal(round(l_res3_min$Parameters[["df2"]], 0), 21)
  expect_equal(signif(l_res3_min$Parameters[["K"]], 7), 2.863636)
  expect_equal(round(l_res3_min$Parameters[["k"]], 0), 6)
  expect_equal(signif(l_res3_min$Parameters[["T2"]], 7), 12.83526)
  expect_equal(signif(l_res3_min$Parameters[["F"]], 7), 6.125919)
  expect_equal(signif(l_res3_min$Parameters[["F.crit"]], 7), 2.574569)
  expect_equal(signif(l_res3_min$Parameters[["p.F"]], 7), 0.008021181)

  expect_equal(signif(l_res3_maj$Parameters[["dm"]], 7), 4.508190)
  expect_equal(round(l_res3_maj$Parameters[["df1"]], 0), 2)
  expect_equal(round(l_res3_maj$Parameters[["df2"]], 0), 21)
  expect_equal(signif(l_res3_maj$Parameters[["K"]], 7), 2.863636)
  expect_equal(round(l_res3_maj$Parameters[["k"]], 0), 6)
  expect_equal(signif(l_res3_maj$Parameters[["T2"]], 7), 121.9427)
  expect_equal(signif(l_res3_maj$Parameters[["F"]], 7), 58.19992)
  expect_equal(signif(l_res3_maj$Parameters[["F.crit"]], 7), 2.574569)
  expect_equal(signif(l_res3_maj$Parameters[["p.F"]], 7), 2.719240e-09)
})

test_that("get_hotelling_successfully_calculates_CIs", {
  l_res <- suppressWarnings(
    get_hotellings(
      m1 = as.matrix(dip11[dip11$Status == "counterfeit", 2:7]),
      m2 = as.matrix(dip11[dip11$Status == "genuine", 2:7]),
      signif = 0.05))

  # <-><-><-><->

  expect_equal(round(l_res[["Parameters"]][["df1"]], 0), 6)
  expect_equal(round(l_res[["Parameters"]][["df2"]], 0), 193)
  expect_equal(signif(l_res[["Parameters"]][["T2"]], 7), 2412.451)
  expect_equal(signif(l_res[["Parameters"]][["F"]], 7), 391.9217)
  expect_equal(signif(l_res[["Parameters"]][["p.F"]], 7), 0.0000000)
  expect_equal(signif(l_res[["Parameters"]][["F.crit"]], 7), 2.145801)
  expect_equal(signif(l_res[["Parameters"]][["t.crit"]], 7), 2.665026)

  # <-><-><-><->

  expect_equal(signif(l_res[["CI"]][["Hotelling"]][, "LCL"], 7),
               c(-0.04432667, -0.5185652, -0.64159649,
                 -2.698094, -1.295233, 1.807197))
  expect_equal(signif(l_res[["CI"]][["Hotelling"]][, "UCL"], 7),
               c(0.3363267, -0.1954348, -0.3044035,
                 -1.751906, -0.6347669, 2.326803))

  # <-><-><-><->

  expect_equal(signif(l_res[["CI"]][["Bonferroni"]][, "LCL"], 7),
               c(0.006434909, -0.4754745, -0.5966305,
                 -2.571916, -1.207157, 1.876489))
  expect_equal(signif(l_res[["CI"]][["Bonferroni"]][, "UCL"], 7),
               c(0.2855651, -0.2385255, -0.3493695,
                 -1.878084, -0.7228426, 2.257511))
})

test_that("get_hotellings_fails", {
  expect_error(suppressWarnings(
    get_hotellings(m1 = dip1[dip1$type == "R", 3:10],
                   m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                   signif = 0.05)),
    "m1 must be provided as matrix")
  expect_error(suppressWarnings(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                   m2 = dip1[dip1$type == "T", 3:10],
                   signif = 0.05)),
    "m2 must be provided as matrix")
  expect_error(suppressWarnings(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:9]),
                   m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                   signif = 0.05)),
    "matrices m1 and m2 must have the same number of columns")
  expect_error(suppressWarnings(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                   m2 = as.matrix(dip1[dip1$type == "T", 3:9]),
                   signif = 0.05)),
    "matrices m1 and m2 must have the same number of columns")
  expect_error(suppressWarnings(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                   m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                   signif = -1)),
    "specify signif")
  expect_error(suppressWarnings(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                   m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                   signif = 9)),
    "specify signif")
})
