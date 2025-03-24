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
  expect_output(print(re1), "Did the Newton-Raphson search converge? Yes",
                fixed = TRUE)
  expect_output(print(re1), "confidence region boundary (CRB)? Yes",
                fixed = TRUE)
  expect_output(print(re1), "Tsong (1996):  Similar", fixed = TRUE)
  expect_output(print(re1), "Hoffelder (2016):  Similar", fixed = TRUE)

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

  expect_s3_class(expect_output(print(re2), "MIMCR"), "mimcr")
  expect_output(print(re2), "Did the Newton-Raphson search converge? No",
                fixed = TRUE)
  expect_output(print(re2), "confidence region boundary (CRB)? NA",
                fixed = TRUE)
  expect_output(print(re2), "Observed upper limit:                 NA",
                fixed = TRUE)
  expect_output(print(re2), "Tsong (1996):  NA", fixed = TRUE)
  expect_output(print(re2), "Hoffelder (2016):  Similar", fixed = TRUE)

  expect_s3_class(expect_output(print(re3), "MIMCR"), "mimcr")
  expect_output(print(re3), "Did the Newton-Raphson search converge? NA",
                fixed = TRUE)
  expect_output(print(re3), "confidence region boundary (CRB)? NA",
                fixed = TRUE)
  expect_output(print(re3), "Observed upper limit:                 NA",
                fixed = TRUE)
  expect_output(print(re3), "Tsong (1996):  NA", fixed = TRUE)
  expect_output(print(re3), "Hoffelder (2016):  Similar", fixed = TRUE)
})
