context("Get time points")

test_that("get_time_points_succeeds", {
  dat1 <- dat2 <- dat3 <- dat4 <- dip2[1:24, 3:8]
  colnames(dat2) <- c("batch", "t_0.0", "t_1.5", "t_3.0", "t_4.5", "t_6.0")
  colnames(dat3) <- c("batch", "t.0.0", "t.1.5", "t.3.0", "t.4.5", "t.6.0")
  colnames(dat4) <- c("batch", "t0.0", "t1.5", "t3.0", "t4.5", "t6.0")

  # <-><-><-><->

  t_res1 <- get_time_points(svec = colnames(dat1)[2:6])
  t_res2 <- get_time_points(svec = colnames(dat2)[2:6])
  t_res3 <- get_time_points(svec = colnames(dat3)[2:6])
  t_res4 <- get_time_points(svec = colnames(dat4)[2:6])

  # <-><-><-><->

  expect_equal(sum(t_res1 %in% c(0.0, 30, 60, 90, 180)), 5)
  expect_equal(sum(t_res2 %in% c(0.0, 1.5, 3.0, 4.5, 6.0)), 5)
  expect_equal(sum(t_res3 %in% c(0.0, 1.5, 3.0, 4.5, 6.0)), 5)
  expect_equal(sum(t_res4 %in% c(0.0, 1.5, 3.0, 4.5, 6.0)), 5)
})

test_that("get_time_points_fails", {
  dat1 <- dat2 <- dip2[1:24, 3:7]
  dat3 <- dip2[1:24, c(3:7, 1)]
  colnames(dat2) <- c("batch.name", "t_0.0", "t_1.5", "t_3.0", "t_4.5")
  colnames(dat3) <- c("batch", "t_0.0", "t_1.5", "t_3.0", "t_4.5", "prod.type")

  # <-><-><-><->

  t_res1 <- get_time_points(svec = colnames(dat1)[1:5])
  t_res2 <- get_time_points(svec = colnames(dat2)[1:5])
  t_res3 <- get_time_points(svec = colnames(dat3)[2:6])

  # <-><-><-><->

  expect_equivalent(t_res1, c(NA, 0, 30, 60, 90))
  expect_equivalent(t_res2, c(NA, 0.0, 1.5, 3.0, 4.5))
  expect_equivalent(t_res3, c(0.0, 1.5, 3.0, 4.5, NA))

  expect_error(
    get_time_points(svec = 1),
    "svec must be string or string vector")
  expect_error(
    get_time_points(svec = 1:5),
    "svec must be string or string vector")
})
