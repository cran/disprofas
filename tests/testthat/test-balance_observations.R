context("Balance observations")

test_that("balance_observations_succeeds", {
  # Generate a subset of dip2 consisting of batches b1 with 6, b2 with 9 and
  # b0 with 12 observations.
  sub1_dip2 <- droplevels(dip2[c(13:18, 25:33, 1:12), ])
  sub2_dip2 <- sub1_dip2

  # Rename batch b0 as batch b3 and reorder the batches as b1, b2 and b3.
  levels(sub2_dip2$batch) <- sub("b0", "b3", levels(sub2_dip2$batch))
  sub2_dip2$batch <- factor(sub2_dip2$batch, levels = c("b1", "b2", "b3"))

  data1 <- droplevels(sub1_dip2[sub1_dip2$batch %in% c("b0", "b1"), ])
  data2 <- droplevels(sub1_dip2[sub1_dip2$batch %in% c("b1", "b2"), ])
  data3 <- droplevels(sub2_dip2[sub2_dip2$batch %in% c("b1", "b3"), ])

  # <-><-><-><->

  b1 <- data1[, "batch"] == levels(data1[, "batch"])[1]
  d_res1 <- balance_observations(data = data1, groups = b1, n_obs = 12)
  d_res2 <- balance_observations(data = data1, groups = b1, n_obs = 6)

  b1 <- data2[, "batch"] == levels(data2[, "batch"])[1]
  d_res3 <- balance_observations(data = data2, groups = b1, n_obs = 12)

  b1 <- data3[, "batch"] == levels(data3[, "batch"])[1]
  d_res4 <- balance_observations(data = data3, groups = b1, n_obs = 12)
  d_res5 <- balance_observations(data = data3, groups = b1, n_obs = 24)

  # <-><-><-><->

  expect_equal(nrow(d_res1[d_res1$batch == "b0", ]), 12)
  expect_equal(nrow(d_res1[d_res1$batch == "b1", ]), 12)

  expect_equal(nrow(d_res2[d_res2$batch == "b0", ]), 12)
  expect_equal(nrow(d_res2[d_res2$batch == "b1", ]), 12)

  expect_equal(nrow(d_res3[d_res3$batch == "b1", ]), 12)
  expect_equal(nrow(d_res3[d_res3$batch == "b2", ]), 12)

  expect_equal(nrow(d_res4[d_res4$batch == "b1", ]), 12)
  expect_equal(nrow(d_res4[d_res4$batch == "b3", ]), 12)

  expect_equal(nrow(d_res5[d_res5$batch == "b1", ]), 24)
  expect_equal(nrow(d_res5[d_res5$batch == "b3", ]), 24)
})

test_that("balance_observations_fails", {
  b1 <- dip1[, "type"] == levels(dip1[, "type"])[1]

  # <-><-><-><->

  expect_error(balance_observations(data = as.matrix(dip1[, 3:9]),
                                    groups = b1, n_obs = 6),
               "data must be provided as data frame")
  expect_error(balance_observations(data = dip1,
                                    groups = 1:12, n_obs = 6),
               "groups must be a logical vector")
  expect_error(balance_observations(data = dip1,
                                    groups = rep(TRUE, 13), n_obs = 6),
               "groups must be a logical vector")
  expect_error(balance_observations(data = dip1,
                                    groups = rep(TRUE, 11), n_obs = 6),
               "groups must be a logical vector")
  expect_error(balance_observations(data = dip1,
                                    groups = b1, n_obs = 6.6),
               "n_obs must be an integer")
})
