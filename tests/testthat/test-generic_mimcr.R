context("Generic summary / print and plot functions for mimcr")

test_that("print_and_thus_summary.mimcr_succeeds", {
  re1 <- mimcr(data = dip3, tcol = 4:6, grouping = "batch", fit_n_obs = FALSE,
              mtad = 10, signif = 0.05, max_trial = 50, bounds = c(1, 85),
              tol = 1e-9)

  re2 <- expect_warning(
    mimcr(data = dip3, tcol = 4:6, grouping = "type", fit_n_obs = TRUE,
          mtad = 10, signif = 0.1, max_trial = 5,  bounds = c(1, 85),
          tol = 1e-15),
    "Newton-Raphson search did not converge")

  re3 <- suppressWarnings(
    mimcr(data = dip2[dip2$batch %in% c("b0", "b4"), ],
          tcol = c(6, 8), grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, bounds = c(1, 85),
          tol = 1e-15))

  # <-><-><-><->

  expect_s3_class(expect_output(print(re1), "MIMCR"), "mimcr")
  expect_output(print(re1), "Yes")
  expect_output(print(re1), "Similar")

  expect_output(summary(re1, digits = 5), "3")
  expect_output(summary(re1, digits = 5), "20")
  expect_output(summary(re1, digits = 5), "0.2384")
  expect_output(summary(re1, digits = 5), "1.8182")
  expect_output(summary(re1, digits = 5), "6")
  expect_output(summary(re1, digits = 5), "0.34101")
  expect_output(summary(re1, digits = 5), "10")
  expect_output(summary(re1, digits = 5), "2.2481")
  expect_output(summary(re1, digits = 5), "1.5438")
  expect_output(summary(re1, digits = 5), "30.323")
  expect_output(summary(re1, digits = 5), "4.8993")
  expect_output(summary(re1, digits = 5), "2.8908e-08")

  expect_equivalent(re2[["Similarity"]]["Tsong"], as.character(NA))
  expect_equivalent(re2[["NR.CI"]][["CI"]][, "LCL"], rep(NA, 3))
  expect_equivalent(re2[["NR.CI"]][["CI"]][, "UCL"], rep(NA, 3))
  expect_equivalent(re2[["NR.CI"]][["converged"]], FALSE)
  expect_equivalent(re2[["NR.CI"]][["points.on.crb"]], NA)
  expect_output(print(re2, digits = 5),
                "Did the Newton-Raphson search converge\\? No")
  expect_output(print(re2, digits = 5),
                "Are the points located on the ",
                "confidence region boundary \\(CRB\\)\\? NA")
  expect_output(print(re2, digits = 5),
                "Observed upper limit:                     NA")
  expect_output(print(re2, digits = 5),
                "Tsong \\(1996\\):  NA ")

  expect_equivalent(re3[["Similarity"]]["Tsong"], as.character(NA))
  expect_equivalent(re3[["NR.CI"]][["CI"]][, "LCL"], rep(NA, 2))
  expect_equivalent(re3[["NR.CI"]][["CI"]][, "UCL"], rep(NA, 2))
  expect_equivalent(re3[["NR.CI"]][["converged"]], NA)
  expect_equivalent(re3[["NR.CI"]][["points.on.crb"]], NA)
  expect_output(print(re3, digits = 5),
                "Did the Newton-Raphson search converge\\? NA")
  expect_output(print(re3, digits = 5),
                "Are the points located on the ",
                "confidence region boundary \\(CRB\\)\\? NA")
  expect_output(print(re3, digits = 5),
                "Observed upper limit:                     NA")
  expect_output(print(re3, digits = 5),
                "Tsong \\(1996\\):  NA ")
})
