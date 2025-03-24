context("Generic summary / print and plot functions for mztia")

test_that("print_and_thus_plot.plot_mztia_succeeds", {
  re1 <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
               reference = "R", response = NULL, alpha = 0.05, pp = 0.99,
               cap = FALSE, bounds = c(0, 100), qs = c(5, 15))
  re2 <- mztia(data = dip5, shape = "long", tcol = 3, grouping = "type",
               reference = "reference", response = "weight", alpha = 0.05,
               pp = 0.99, cap = FALSE, bounds = c(0, 100), qs = c(5, 15) / 100)

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

test_that("print_and_thus_summary.mztia_succeeds", {
  re <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
              reference = "R", response = NULL, na_rm = FALSE, alpha = 0.05,
              pp = 0.99, cap = FALSE, bounds = c(0, 100), qs = c(5, 15))

  # <-><-><-><->

  expect_s3_class(expect_output(print(re), "Martinez & Zhao"), "mztia")
  expect_output(print(re, digits = 5),
                "Time   Mean    LTL     UTL S1.LTL  S1.UTL S2.LTL  S2.UTL",
                fixed = TRUE)
  expect_output(print(re, digits = 5),
                "15 67.275 56.904  77.646 51.904  82.646 41.904  92.646",
                fixed = TRUE)

  expect_output(print(re), "S1: level 1 boundary (LTL - 5) or (UTL + 5)",
                fixed = TRUE)
  expect_output(print(re), "S2: level 2 boundary (LTL - 15) or (UTL + 15)",
                fixed = TRUE)
})
