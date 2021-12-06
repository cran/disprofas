context("Generating Jackknife values")

test_that("get_jackknife_values_succeeds", {
  l_jack <- get_jackknife_values(grouping = "batch", stat_fun = get_f2,
                               data = dip2[dip2$batch %in% c("b0", "b4"), ],
                               tcol = 5:8)

  # <-><-><-><->

  expect_equal(round(l_jack$theta.hat, 5), 50.07187)
  expect_equal(round(l_jack$theta.jack, 5), 50.05253)
  expect_equal(round(l_jack$jack.se, 5), 0.92031)
  expect_equal(round(l_jack$jack.bias, 5), -0.01934)
  expect_equal(round(l_jack$loo.values, 5),
               c(49.98991, 49.79446, 50.38612, 49.79446, 50.28639, 50.28639,
                 49.89197, 50.08829, 50.08829, 50.48630, 49.50450, 50.28639))
  expect_equal(round(l_jack$pseudo.values, 5),
               c(50.97338, 53.12337, 46.61507, 53.12337, 47.71205, 47.71205,
                 52.05077, 49.89116, 49.89116, 45.51309, 56.31284, 47.71205))
})

test_that("get_jackknife_values_fails", {
  expect_error(
    get_jackknife_values(grouping = "batch", stat_fun = get_f2,
                         data = as.matrix(dip2[dip2$batch %in% c("b0", "b4"),
                                               5:8]), tcol = 5:8),
    "data must be provided as data frame")
  expect_error(
    get_jackknife_values(grouping = "lot", stat_fun = get_f2,
                         data = dip2[dip2$batch %in% c("b0", "b4"), ],
                         tcol = 5:8),
    "grouping variable was not found")
})
