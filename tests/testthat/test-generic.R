context("Generic summary and print (and plot) functions")

test_that("plot.bootstrap_f2_succeeds", {
  re <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                     tcol = 5:8, grouping = "batch",
                     R = 200, new_seed = 421, use_EMA = "no")

  # <-><-><-><->

  expect_s3_class(expect_output(plot(re), "Shah"), "bootstrap_f2")
  expect_output(plot(re, digits = 5), "48.646")
})

test_that("summary.bootstrap_f2_succeeds", {
  re <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                     tcol = 5:8, grouping = "batch",
                     R = 200, new_seed = 421, use_EMA = "no")

  # <-><-><-><->

  expect_s3_class(expect_output(summary(re), "Shah"), "bootstrap_f2")
  expect_output(summary(re), "STRATIFIED BOOTSTRAP")
  expect_output(summary(re), "BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS")

  expect_output(summary(re, digits = 5), "50.072")
  expect_output(summary(re, digits = 5), "48.646")
})

test_that("print.bootstrap_f2_succeeds", {
  re <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                     tcol = 5:8, grouping = "batch",
                     R = 200, new_seed = 421, use_EMA = "no")

  # <-><-><-><->

  expect_s3_class(expect_output(print(re), "Shah"), "bootstrap_f2")
  expect_output(print(re), "STRATIFIED BOOTSTRAP")
  expect_output(print(re), "BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS")

  expect_output(print(re, digits = 5), "50.072")
  expect_output(print(re, digits = 5), "48.646")
})

test_that("summary.mimcr_succeeds", {
  re <- mimcr(data = dip3, tcol = 4:6, grouping = "batch", fit_n_obs = FALSE,
              mtad = 10, signif = 0.05, max_trial = 50, bounds = c(1, 85),
              tol = 1e-9)

  # <-><-><-><->

  expect_s3_class(expect_output(summary(re), "MIMCR"), "mimcr")
  expect_output(summary(re), "Yes")
  expect_output(summary(re), "Similar")

  expect_output(summary(re, digits = 5), "1.5438")
  expect_output(summary(re, digits = 5), "2.8908e-08")
})

test_that("print.mimcr_succeeds", {
  re <- mimcr(data = dip3, tcol = 4:6, grouping = "batch", fit_n_obs = FALSE,
              mtad = 10, signif = 0.05, max_trial = 50, bounds = c(1, 85),
              tol = 1e-9)

  # <-><-><-><->

  expect_s3_class(expect_output(print(re), "MIMCR"), "mimcr")
  expect_output(print(re), "Yes")
  expect_output(print(re), "Similar")

  expect_output(print(re, digits = 5), "1.5438")
  expect_output(print(re, digits = 5), "2.8908e-08")
})


test_that("plot.plot_mztia_succeeds", {
  re1 <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
               reference = "R", response = NULL, alpha = 0.05, P = 0.99,
               cap = FALSE, bounds = c(0, 100), QS = c(5, 15))
  re2 <- mztia(data = dip5, shape = "long", tcol = 3, grouping = "type",
               reference = "reference", response = "weight", alpha = 0.05,
               P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15) / 100)

  # <-><-><-><->

  ggre1_1 <- expect_output(plot_mztia(re1), regexp = NA)
  ggre1_2 <- expect_invisible(suppressWarnings(plot(x = ggre1_1)))

  ggre2_1 <- expect_output(plot_mztia(re2), regexp = NA)
  ggre2_2 <- expect_invisible(suppressWarnings(plot(x = ggre2_1)))

  # <-><-><-><->

  expect_s3_class(ggre1_2, "plot_mztia")
  expect_length(ggre1_2, 4)
  expect_s3_class(ggre1_2$Graph, c("gg", "ggplot"))
  expect_equal(
    ggre1_2$Graph$scales$scales[[1]]$labels,
    c("Obs R", "Obs T", "Mean", "TL", "TL ± S1 (5%)", "TL  ± S2 (15%)"))

  expect_s3_class(ggre2_2, "plot_mztia")
  expect_length(ggre2_2, 4)
  expect_s3_class(ggre2_2$Graph, c("gg", "ggplot"))
  expect_length(ggre2_2$Graph$scales$scales, 0)
  expect_equivalent(class(ggre2_2$Graph$layers[[1]]$position),
                    c("PositionJitter", "Position", "ggproto", "gg"))
})

test_that("print.plot_mztia_succeeds", {
  re1 <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
               reference = "R", response = NULL, alpha = 0.05, P = 0.99,
               cap = FALSE, bounds = c(0, 100), QS = c(5, 15))
  re2 <- mztia(data = dip5, shape = "long", tcol = 3, grouping = "type",
               reference = "reference", response = "weight", alpha = 0.05,
               P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15) / 100)

  # <-><-><-><->

  ggre1_1 <- expect_output(plot_mztia(re1), regexp = NA)
  ggre1_2 <- expect_invisible(suppressWarnings(print(x = ggre1_1)))

  ggre2_1 <- expect_output(plot_mztia(re2), regexp = NA)
  ggre2_2 <- expect_invisible(suppressWarnings(print(x = ggre2_1)))

  # <-><-><-><->

  expect_s3_class(ggre1_2, "plot_mztia")
  expect_length(ggre1_2, 4)
  expect_s3_class(ggre1_2$Graph, c("gg", "ggplot"))
  expect_equal(
    ggre1_2$Graph$scales$scales[[1]]$labels,
    c("Obs R", "Obs T", "Mean", "TL", "TL ± S1 (5%)", "TL  ± S2 (15%)"))

  expect_s3_class(ggre2_2, "plot_mztia")
  expect_length(ggre2_2, 4)
  expect_s3_class(ggre2_2$Graph, c("gg", "ggplot"))
  expect_length(ggre2_2$Graph$scales$scales, 0)
  expect_equivalent(class(ggre2_2$Graph$layers[[1]]$position),
                    c("PositionJitter", "Position", "ggproto", "gg"))
})

test_that("summary.mztia_succeeds", {
  re <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
              reference = "R", alpha = 0.05, P = 0.99, cap = FALSE)

  # <-><-><-><->

  expect_s3_class(expect_output(summary(re), "Martinez & Zhao"), "mztia")
  expect_output(summary(re), "Time")
  expect_output(summary(re), "S2.UTL")

  expect_output(summary(re, digits = 5), "15")
  expect_output(summary(re, digits = 5), "92.646")
})

test_that("print.mztia_succeeds", {
  re <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
              reference = "R", alpha = 0.05, P = 0.99, cap = FALSE)

  # <-><-><-><->

  expect_s3_class(expect_output(print(re), "Martinez & Zhao"), "mztia")
  expect_output(print(re), "Time")
  expect_output(print(re), "S2.UTL")

  expect_output(print(re, digits = 5), "15")
  expect_output(print(re, digits = 5), "92.646")
})
