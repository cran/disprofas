context("Get similarity limits")

test_that("get_sim_lim_succeeds", {
  hs <- get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                       m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                       signif = 0.1)
  t_res <- get_sim_lim(mtad = 10, hs)

  # <-><-><-><->

  expect_equivalent(signif(t_res["DM"], 7), 26.48562)
  expect_equivalent(round(t_res["df1"], 0), 8)
  expect_equivalent(round(t_res["df2"], 0), 3)
  expect_equivalent(signif(t_res["K"], 7), 0.1125000)
  expect_equivalent(round(t_res["k"], 0), 3)
  expect_equivalent(signif(t_res["T2"], 7), 2104.464)
  expect_equivalent(signif(t_res["F"], 7), 78.91739)
  expect_equivalent(signif(t_res["ncp.Hoffelder"], 7), 1132.247)
  expect_equivalent(signif(t_res["F.crit"], 7), 5.251671)
  expect_equivalent(signif(t_res["F.crit.Hoffelder"], 7), 68.08586)
  expect_equivalent(signif(t_res["p.F"], 7), 0.002116258)
  expect_equivalent(signif(t_res["p.F.Hoffelder"], 7), 0.1449045)
  expect_equivalent(signif(t_res["Sim.Limit"], 7), 19.42719)
})

test_that("get_sim_lim_fails", {
  hs <- get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                       m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                       signif = 0.1)
  hs1 <- hs
  names(hs1) <- c("Variables", "Spool", "covs", "means")

  # <-><-><-><->

  expect_error(
    get_sim_lim(mtad = 10, lhs = hs1),
    "lhs must be a list returned by get_hotellings")
  expect_error(
    get_sim_lim(mtad = 10, lhs = hs[[1]]),
    "lhs must be a list returned by get_hotellings")
  expect_error(
    get_sim_lim(mtad = -1, lhs = hs),
    "specify mtad")
  expect_error(
    get_sim_lim(mtad = 51, lhs = hs),
    "specify mtad")
})
