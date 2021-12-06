context("Plot of Dissolution Profile Tolerance Intervals")

test_that("plot_mztia_succeeds_with_df_shape_wide", {
  re1 <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
               reference = "R", response = NULL, alpha = 0.05, P = 0.99,
               cap = FALSE, bounds = c(0, 100), QS = c(5, 15))
  re2 <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
               reference = "R", response = NULL, alpha = 0.05, P = 0.99,
               cap = TRUE, bounds = c(0, 100), QS = c(5, 15))

  # <-><-><-><->

  gg1 <- expect_output(plot_mztia(re1), regexp = NA)
  gg2 <- expect_output(plot_mztia(re2), regexp = NA)

  # <-><-><-><->

  expect_length(gg1, 4)
  expect_s3_class(gg1$Graph, c("gg", "ggplot"))
  expect_that(gg1$Data, equals(gg1[["Graph"]]$data))
  expect_equal(
    gg1$Graph$scales$scales[[1]]$labels,
    c("Obs R", "Obs T", "Mean", "TL", "TL ± S1 (5%)", "TL  ± S2 (15%)"))

  expect_length(gg2, 4)
  expect_s3_class(gg2$Graph, c("gg", "ggplot"))
  expect_that(gg2$Data, equals(gg2[["Graph"]]$data))
  expect_equal(
    gg2$Graph$scales$scales[[1]]$labels,
    c("Obs R", "Obs T", "Mean", "TL", "TL ± S1 (5%)", "TL  ± S2 (15%)"))
})

test_that("plot_mztia_succeeds_with_df_shape_long", {
  re1 <- mztia(data = dip5, shape = "long", tcol = 3, grouping = "type",
               reference = "reference", response = "weight", alpha = 0.05,
               P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15) / 100)

  # <-><-><-><->

  gg1 <- expect_output(plot_mztia(re1), regexp = NA)

  # <-><-><-><->

  expect_length(gg1, 4)
  expect_s3_class(gg1$Graph, c("gg", "ggplot"))
  expect_that(gg1$Data, equals(gg1[["Graph"]]$data))
  expect_length(gg1$Graph$scales$scales, 0)
  expect_equivalent(class(gg1$Graph$layers[[1]]$position),
                    c("PositionJitter", "Position", "ggproto", "gg"))
})

test_that("plot_mztia_fails", {
  re <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
              reference = "R", alpha = 0.05, P = 0.99, cap = FALSE)

  ree <- re
  class(ree) <- "manip"

  # <-><-><-><->

  expect_error(
    plot_mztia(ree),
    "parameter x must be an object of class mztia")
})
