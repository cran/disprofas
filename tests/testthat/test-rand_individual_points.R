context("Randomise individual profile test points")

test_that("rand_indiv_points_succeeds", {
  suppressWarnings(RNGkind(sample.kind = "Rounding"))

  # <-><-><-><->
  set.seed(1973)
  res <- rand_indiv_points(data = dip1, mle = list(6, 3:10))

  # <-><-><-><->

  expect_equal(res$t.30, c(80.18, 78.99, 76.92, 78.45, 78.45, 77.77,
                           65.69, 69.44, 65.25, 71.51, 71.51, 71.51))

  # <-><-><-><->
  RNGkind(sample.kind = "default")
})

test_that("rand_indiv_points_fails", {
  suppressWarnings(RNGkind(sample.kind = "Rounding"))

  # <-><-><-><->
  ok <- 1:4

  mle <- list()
  mle[[length(mle) + 1]] <- nrow(dip2[dip2$batch %in% c("b0", "b4"), ]) / 2
  mle[[length(mle) + 1]] <- c(5:8)[ok]

  mle2 <- mle3 <- mle4 <- mle5 <- mle6 <- mle7 <- mle8 <- mle9 <- mle10 <- mle

  mle2[[1]] <- "12"
  mle3[[1]] <- 1:3
  mle4[[1]] <- 1.1
  mle5[[1]] <- 5
  mle6[[2]] <- "1:12"
  mle7[[2]] <- 1
  mle8[[2]] <- c(5.5, 6.6, 7.7, 8.8)
  mle9[[2]] <- 6:9
  mle10[[2]] <- 3:5

  # <-><-><-><->

  expect_error(
    rand_indiv_points(data = as.matrix(dip2[dip2$batch %in% c("b0", "b4"),
                                               5:8]), mle = mle),
    "data must be provided as data frame")
  expect_error(
    rand_indiv_points(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                      mle = "mle"),
    "mle must be a list of length 2")
  expect_error(
    rand_indiv_points(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                      mle = list(1, 2, 3)),
    "mle must be a list of length 2")
  expect_error(
    rand_indiv_points(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                         mle = mle2),
    "mle must be an integer value")
  expect_error(
    rand_indiv_points(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                      mle = mle3),
    "mle must be an integer value")
  expect_error(
    rand_indiv_points(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                      mle = mle4),
    "mle must be an integer value")
  expect_error(
    rand_indiv_points(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                      mle = mle5),
    "mle must be half of the number")
  expect_error(
    rand_indiv_points(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                      mle = mle6),
    "The second element of mle must be an integer vector")
  expect_error(
    rand_indiv_points(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                      mle = mle7),
    "The second element of mle must be an integer vector")
  expect_error(
    rand_indiv_points(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                      mle = mle8),
    "The second element of mle must be an integer vector")
  expect_error(
    rand_indiv_points(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                      mle = mle9),
    "Some columns specified by the second element of mle")
  expect_error(
    rand_indiv_points(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                      mle = mle10),
    "Some columns specified by the second element of mle")

  # <-><-><-><->
  RNGkind(sample.kind = "default")
})
