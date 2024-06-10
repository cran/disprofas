context("Generic summary / print and plot functions for bootstrap_f2")

test_that("plot.bootstrap_f2_succeeds", {
  re <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                     tcol = 5:8, grouping = "batch",
                     rr = 200, new_seed = 421, use_ema = "no")

  # <-><-><-><->

  expect_s3_class(expect_output(plot(re), "Shah"), "bootstrap_f2")
  expect_output(plot(re, digits = 5), "48.646")
})

test_that("print_and_thus_summary.bootstrap_f2_succeeds", {
  re <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                     tcol = 5:8, grouping = "batch",
                     rr = 200, new_seed = 421, use_ema = "no")

  # <-><-><-><->

  expect_s3_class(expect_output(print(re), "Shah"), "bootstrap_f2")
  expect_output(print(re), "STRATIFIED BOOTSTRAP")
  expect_output(print(re), "BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS")

  expect_output(print(re, digits = 5), "50.072")
  expect_output(print(re, digits = 5), "48.646")
})
