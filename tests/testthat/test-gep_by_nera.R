context("Get ellipsoid points by Newton-Raphson search")

test_that("gep_by_nera_succeeds", {
  tico <- 3:10
  b1 <- dip1$type == "R"

  # Hotelling's T2 statistics
  l_hs <- get_T2_two(m1 = as.matrix(dip1[b1, tico]),
                     m2 = as.matrix(dip1[!b1, tico]),
                     signif = 0.05)

  # <-><-><-><->

  res <-  gep_by_nera(n_p = as.numeric(l_hs[["Parameters"]]["df1"]),
                      kk = as.numeric(l_hs[["Parameters"]]["K"]),
                      mean_diff = l_hs[["means"]][["mean.diff"]],
                      m_vc = l_hs[["S.pool"]],
                      ff_crit = as.numeric(l_hs[["Parameters"]]["F.crit"]),
                      y = rep(1, times = l_hs[["Parameters"]]["df1"] + 1),
                      max_trial = 100, tol = 1e-15)

  # <-><-><-><->

  expect_equivalent(signif(res[["points"]][, 1], 7),
                    c(-15.76001, -13.65017, -11.66895, -9.842937, -6.663218,
                      -0.4634318, 2.252855, 3.324957, -17.66200))
})

test_that("gep_by_nera_fails", {
  tico <- 3:10
  b1 <- dip1$type == "R"

  n_tp <- length(tico)
  n_b1 <- length(dip1[b1, "type"])
  n_b2 <- length(dip1[!b1, "type"])
  df_b1 <- n_tp
  df_b2 <- n_b1 + n_b2 - n_tp - 1
  kk_limit <- (n_b2 * n_b1) / (n_b2 + n_b1)
  kk <- kk_limit * df_b2 / (df_b1 * (n_b2 + n_b1 - 2))

  mean_b1 <- apply(X = dip1[b1, tico], MARGIN = 2, FUN = mean)
  mean_b2 <- apply(X = dip1[!b1, tico], MARGIN = 2, FUN = mean)
  mean_diff <- mean_b2 - mean_b1

  m_vc_b1 <- cov(dip1[b1, tico])
  m_vc_b2 <- cov(dip1[!b1, tico])
  m_vc <- ((n_b1 - 1) * m_vc_b1 + (n_b2 - 1) * m_vc_b2) / (n_b1 + n_b2 - 2)

  ff_crit <- qf(p = (1 - 0.05), df1 = df_b1, df2 = df_b2)
  y_b1 <- rep(1, times = (n_tp + 1))

  # <-><-><-><->

  expect_error(
    gep_by_nera(n_p = "n_tp", kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = ff_crit, y = y_b1, max_trial = 50, tol = 1e-9),
    "n_p must be a positive integer")
  expect_error(
    gep_by_nera(n_p = c(n_tp, n_tp), kk = kk, mean_diff = mean_diff,
                m_vc = m_vc, ff_crit = ff_crit, y = y_b1, max_trial = 50,
                tol = 1e-9),
    "n_p must be a positive integer")
  expect_error(
    gep_by_nera(n_p = -n_tp, kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = ff_crit, y = y_b1, max_trial = 50, tol = 1e-9),
    "n_p must be a positive integer")
  expect_error(
    gep_by_nera(n_p = n_tp / 3, kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = ff_crit, y = y_b1, max_trial = 50, tol = 1e-9),
    "n_p must be a positive integer")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = "kk", mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = ff_crit, y = y_b1, max_trial = 50, tol = 1e-9),
    "kk must be a non-negative numeric")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = c(kk, kk), mean_diff = mean_diff,
                m_vc = m_vc, ff_crit = ff_crit, y = y_b1, max_trial = 50,
                tol = 1e-9),
    "kk must be a non-negative numeric")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = -kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = ff_crit, y = y_b1, max_trial = 50, tol = 1e-9),
    "kk must be a non-negative numeric")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = as.character(mean_diff),
                m_vc = m_vc, ff_crit = ff_crit, y = y_b1, max_trial = 50,
                tol = 1e-9),
    "mean_diff must be a numeric vector")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = c(mean_diff, mean_diff),
                m_vc = m_vc, ff_crit = ff_crit, y = y_b1, max_trial = 50,
                tol = 1e-9),
    "mean_diff must be a numeric vector")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = mean_diff,
                m_vc = as.vector(m_vc), ff_crit = ff_crit, y = y_b1,
                max_trial = 50, tol = 1e-9),
    "m_vc must be a matrix")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = mean_diff,
                m_vc = m_vc[1:2, 1:2], ff_crit = ff_crit, y = y_b1,
                max_trial = 50, tol = 1e-9),
    "m_vc must be a matrix")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = "ff_crit", y = y_b1, max_trial = 50, tol = 1e-9),
    "ff_crit must be a non-negative numeric")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = c(ff_crit, ff_crit), y = y_b1, max_trial = 50,
                tol = 1e-9),
    "ff_crit must be a non-negative numeric")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = -ff_crit, y = y_b1, max_trial = 50, tol = 1e-9),
    "ff_crit must be a non-negative numeric")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = ff_crit, y = as.character(y_b1), max_trial = 50,
                tol = 1e-9),
    "y must be a numeric vector")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = ff_crit, y = y_b1[1:n_tp], max_trial = 50,
                tol = 1e-9),
    "y must be a numeric vector")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = ff_crit, y = y_b1, max_trial = "50", tol = 1e-9),
    "max_trial must be a positive integer")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = ff_crit, y = y_b1, max_trial = c(50, 50),
                tol = 1e-9),
    "max_trial must be a positive integer")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = ff_crit, y = y_b1, max_trial = 50.5, tol = 1e-9),
    "max_trial must be a positive integer")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = ff_crit, y = y_b1, max_trial = -50, tol = 1e-9),
    "max_trial must be a positive integer")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = ff_crit, y = y_b1, max_trial = 50, tol = "1e-9"),
    "tol must be a non-negative numeric")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = ff_crit, y = y_b1, max_trial = 50,
                tol = c(1e-9, 1e-9)),
    "tol must be a non-negative numeric")
  expect_error(
    gep_by_nera(n_p = n_tp, kk = kk, mean_diff = mean_diff, m_vc = m_vc,
                ff_crit = ff_crit, y = y_b1, max_trial = 50, tol = -1e-9),
    "tol must be a non-negative numeric")
})

test_that("gep_by_nera_does_not_converge", {
  tico <- 3:10
  b1 <- dip1$type == "R"

  dip1m <- dip1
  tmp1 <- dip1[b1, "t.5"]
  tmp2 <- dip1[!b1, "t.5"]
  dip1m$t.90 <- c(tmp2, tmp1)

  # Hotelling's T2 statistics
  l_hs <- get_T2_two(m1 = as.matrix(dip1m[b1, tico]),
                     m2 = as.matrix(dip1m[!b1, tico]),
                     signif = 0.05)

  # <-><-><-><->

  expect_warning(
    gep_by_nera(n_p = as.numeric(l_hs[["Parameters"]]["df1"]),
                kk = as.numeric(l_hs[["Parameters"]]["K"]),
                mean_diff = l_hs[["means"]][["mean.diff"]],
                m_vc = l_hs[["S.pool"]],
                ff_crit = as.numeric(l_hs[["Parameters"]]["F.crit"]),
                y = rep(1, times = l_hs[["Parameters"]]["df1"] + 1),
                max_trial = 10, tol = 1e-15),
    "Newton-Raphson search did not converge"
  )
})
