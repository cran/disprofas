context("Bootstrap f2")

test_that("bootstrap_f2_rand_mode_complete_results_match", {
  suppressWarnings(RNGkind(sample.kind = "Rounding"))

  # <-><-><-><->

  l_boot <-
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ], tcol = 5:8,
                 grouping = "batch", rand_mode = "complete", R = 200,
                 new_seed = 421, use_EMA = "no")

  # <-><-><-><->

  expect_equal(signif(l_boot[["Boot"]]$t0, 7), 50.07187)
  expect_equal(signif(l_boot$BCa_CI, 7), c(48.87966, 51.95272))
  expect_equal(signif(l_boot$ShahBCa_CI, 7), c(48.83082, 51.74653))
  expect_equivalent(l_boot$Profile.TP, c(30, 60, 90, 180))
  expect_equal(signif(l_boot$L, 7), c(49.98991, 49.79446, 50.38612, 49.79446,
                                      50.28639, 50.28639, 49.89197, 50.08829,
                                      50.08829, 50.48630, 49.50450, 50.28639))

  # <-><-><-><->
  RNGkind(sample.kind = "default")
})

test_that("bootstrap_f2_rand_mode_individual_results_match", {
  suppressWarnings(RNGkind(sample.kind = "Rounding"))

  # <-><-><-><->

  l_boot <-
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ], tcol = 5:8,
                 grouping = "batch", rand_mode = "individual", R = 200,
                 new_seed = 421, use_EMA = "no")

  # <-><-><-><->

  expect_equal(signif(l_boot[["Boot"]]$t0, 7), 50.07187)
  expect_equal(signif(l_boot$BCa_CI, 7), c(48.74848, 52.53160))
  expect_equal(signif(l_boot$ShahBCa_CI, 7), c(48.68587, 52.37645))
  expect_equivalent(l_boot$Profile.TP, c(30, 60, 90, 180))
  expect_equal(signif(l_boot$L, 7), c(49.98991, 49.79446, 50.38612, 49.79446,
                                      50.28639, 50.28639, 49.89197, 50.08829,
                                      50.08829, 50.48630, 49.50450, 50.28639))

  # <-><-><-><->
  RNGkind(sample.kind = "default")
})

test_that("bootstrap_f2_fails", {
  suppressWarnings(RNGkind(sample.kind = "Rounding"))

  # <-><-><-><->
  tmp0 <- dip2
  tmp0$t.30 <- as.factor(tmp0$t.30)

  tmp1 <- dip2
  tmp1$batch <- as.character(tmp1$batch)

  # <-><-><-><->

  expect_error(
    bootstrap_f2(data = as.matrix(dip2[dip2$batch %in% c("b0", "b4"), 5:8]),
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "data must be provided as data frame")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = "tcol", grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "tcol must be an integer vector")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:6, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "tcol must be an integer vector")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = c(5.5, 6.5, 7.5), grouping = "batch",
                 rand_mode = "complete", R = 200, each = 12, new_seed = 421,
                 confid = 0.9, use_EMA = "no", bounds = c(1, 85)),
    "tcol must be an integer vector")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:9, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "Some columns specified by tcol were not found")
  expect_error(
    bootstrap_f2(data = tmp0[tmp0$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "Some columns specified by tcol are not numeric")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = 5, rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "grouping must be string")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "lot", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "grouping variable was not found")
  expect_error(
    bootstrap_f2(data = tmp1[tmp1$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "grouping variable's column in data")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b3", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "number of levels in column")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "alle",
                 R = "R", each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "specify rand_mode either as \"complete\" or \"individual\"")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = "R", each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "R must be an integer")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = c(200, 2000), each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "R must be an integer")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 1.1, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "R must be an integer")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = "each", new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "each must be an integer")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = c(12, 18), new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "each must be an integer")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12.2, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "each must be an integer")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = "new.seed", confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "new_seed must be an integer")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = c(100, 421), confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "new_seed must be an integer")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 11.1, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85)),
    "new_seed must be an integer")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0,
                 use_EMA = "no", bounds = c(1, 85)),
    "specify confid")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 9,
                 use_EMA = "no", bounds = c(1, 85)),
    "specify confid")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "maybe", bounds = c(1, 85)),
    "specify use_EMA either as \"yes\" or \"no\" or \"ignore\"")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c("1", "85")),
    "bounds must be a numeric vector of length 2")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 85, 100)),
    "bounds must be a numeric vector of length 2")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(85, 1)),
    "specify bounds in the form")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(-1, 85)),
    "specify bounds in the range")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "no", bounds = c(1, 101)),
    "specify bounds in the range")
  expect_error(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b1"), ],
                 tcol = 6:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "yes", bounds = c(1, 85)),
    "According to EMA the profiles")

  # <-><-><-><->
  RNGkind(sample.kind = "default")
})

test_that("bootstrap_f2_warns", {
  suppressWarnings(RNGkind(sample.kind = "Rounding"))

  # <-><-><-><->
  tmp <- rbind(dip2[dip2$batch == "b0", ],
               dip2[dip2$batch == "b4" & dip2$tablet %in% as.character(1:6), ])

  # <-><-><-><->

  expect_warning(
    bootstrap_f2(data = tmp,
                 tcol = 5:8, grouping = "batch", rand_mode = "complete",
                 R = 200, each = 12, new_seed = 421, confid = 0.9,
                 use_EMA = "yes", bounds = c(1, 85)),
    "The two groups to be compared")
  expect_error(
    expect_warning(
      bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b1"), ],
                   tcol = 6:8, grouping = "batch", rand_mode = "complete",
                   R = 200, each = 12, new_seed = 421, confid = 0.9,
                   use_EMA = "no", bounds = c(1, 85)),
      "The profiles should comprise"),
    "tcol must be an integer vector")
  expect_error(
    expect_warning(
      bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                   tcol = 5:7, grouping = "batch", rand_mode = "complete",
                   R = 200, each = 12, new_seed = 421, confid = 0.9,
                   use_EMA = "no", bounds = c(1, 55)),
      "according to EMA"),
    "tcol must be an integer vector")

  # <-><-><-><->
  RNGkind(sample.kind = "default")
})
