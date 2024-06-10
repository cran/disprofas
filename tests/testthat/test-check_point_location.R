context("Check if points on ellipsoid lie on confidence region bounds")

test_that("check_point_location_succeeds", {
  tico <- 3:10
  b1 <- dip1$type == "R"

  # Hotelling's T2 statistics
  l_hs <- get_T2_two(m1 = as.matrix(dip1[b1, tico]),
                     m2 = as.matrix(dip1[!b1, tico]),
                     signif = 0.05)

  # Get points by aid of the Method of Lagrange Multipliers (MLM) and by
  # Newton-Raphson (nera) optimisation
  tmp <-  gep_by_nera(n_p = as.numeric(l_hs[["Parameters"]]["df1"]),
                      kk = as.numeric(l_hs[["Parameters"]]["K"]),
                      mean_diff = l_hs[["means"]][["mean.diff"]],
                      m_vc = l_hs[["S.pool"]],
                      ff_crit = as.numeric(l_hs[["Parameters"]]["F.crit"]),
                      y = rep(1, times = l_hs[["Parameters"]]["df1"] + 1),
                      max_trial = 100, tol = 1e-15)

  # <-><-><-><->

  res <- check_point_location(lpt = tmp, lhs = l_hs)

  # <-><-><-><->

  expect_equivalent(tmp$points.on.crb, NA)
  expect_equivalent(res$points.on.crb, TRUE)
})

test_that("check_point_location_fails", {
  tico <- 3:10
  b1 <- dip1$type == "R"

  # Hotelling's T2 statistics
  l_hs <- get_T2_two(m1 = as.matrix(dip1[b1, tico]),
                     m2 = as.matrix(dip1[!b1, tico]),
                     signif = 0.05)

  # Get points by aid of the Method of Lagrange Multipliers (MLM) and by
  # Newton-Raphson (nera) optimisation
  tmp <-  gep_by_nera(n_p = as.numeric(l_hs[["Parameters"]]["df1"]),
                      kk = as.numeric(l_hs[["Parameters"]]["K"]),
                      mean_diff = l_hs[["means"]][["mean.diff"]],
                      m_vc = l_hs[["S.pool"]],
                      ff_crit = as.numeric(l_hs[["Parameters"]]["F.crit"]),
                      y = rep(1, times = l_hs[["Parameters"]]["df1"] + 1),
                      max_trial = 100, tol = 1e-15)

  # Prepare non-standard lists
  l_hs_mod <- l_hs[c(1:3)]
  tmp_mod <- tmp[c(1:2, 4:5)]

  # <-><-><-><->

  expect_error(
    check_point_location(lpt = "lpt", lhs = l_hs),
    "lpt must be a list ")
  expect_error(
    check_point_location(lpt = tmp_mod, lhs = l_hs),
    "lpt must be a list ")
  expect_error(
    check_point_location(lpt = tmp, lhs = "l_hs"),
    "lhs must be a list ")
  expect_error(
    check_point_location(lpt = tmp, lhs = l_hs_mod),
    "lhs must be a list ")
})
