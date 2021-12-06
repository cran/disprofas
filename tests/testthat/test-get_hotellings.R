context("Get Hotelling's T2 statistics")

test_that("get_hotellings_succeeds", {
  l_res <- get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                          m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                          signif = 0.1)

  # <-><-><-><->

  expect_equal(signif(l_res$Parameters[["DM"]], 7), 26.48562)
  expect_equal(round(l_res$Parameters[["df1"]], 0), 8)
  expect_equal(round(l_res$Parameters[["df2"]], 0), 3)
  expect_equal(signif(l_res$Parameters[["K"]], 7), 0.1125000)
  expect_equal(round(l_res$Parameters[["k"]], 0), 3)
  expect_equal(signif(l_res$Parameters[["T2"]], 7), 2104.464)
  expect_equal(signif(l_res$Parameters[["F"]], 7), 78.91739)
  expect_equal(signif(l_res$Parameters[["F.crit"]], 7), 5.251671)
  expect_equal(signif(l_res$Parameters[["p.F"]], 7), 0.002116258)
})

test_that("get_hotellings_fails", {
  expect_error(
    get_hotellings(m1 = dip1[dip1$type == "R", 3:10],
                   m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                   signif = 0.05),
    "m1 must be provided as matrix")
  expect_error(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                   m2 = dip1[dip1$type == "T", 3:10],
                   signif = 0.05),
    "m2 must be provided as matrix")
  expect_error(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:9]),
                   m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                   signif = 0.05),
    "m1 and m2 must have the same dimensions")
  expect_error(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                   m2 = as.matrix(dip1[dip1$type == "T", 3:9]),
                   signif = 0.05),
    "m1 and m2 must have the same dimensions")
  expect_error(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                   m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                   signif = -1),
    "specify signif")
  expect_error(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                   m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                   signif = 9),
    "specify signif")
})
