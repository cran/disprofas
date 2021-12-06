context("Dissolution Profile Tolerance Intervals")

# The tests use the values published in the SAS/QC(R) 13.1: User's Guide,
# Chapter 5 (The Capability Procedure), details on pp 421-424. The data set is
# described on page 199: The fluid weights of 100 drink cans are measured in
# ounces. The filling process is assumed to be in statistical control. The
# measurements are saved in a SAS data set named Cans. The tolerance intervals
# calculated by the capability procedure are presented on page 416 in
# Figure 5.25. The data are put into a data frame so that the can be used for
# analysis by the mztia function.

test_that("mztia_succeeds_CAPABILITY", {
  m_alpha_P <- matrix(c(rep(c(0.01, 0.05, 0.1), each = 3),
                        1 - rep(c(0.1, 0.05, 0.01), times = 3)),
                      ncol = 2, byrow = FALSE)

  ll <-
    apply(m_alpha_P, MARGIN = 1, FUN = function(x)
      mztia(data = dip5, shape = "long", tcol = 1, grouping = "type",
            reference = "reference", response = "weight", alpha = x[1],
            P = x[2], cap = FALSE, bounds = c(0, 100), QS = c(5, 15))
      [["Data"]][102, "weight"])
  ul <-
    apply(m_alpha_P, MARGIN = 1, FUN = function(x)
      mztia(data = dip5, shape = "long", tcol = 1, grouping = "type",
            reference = "reference", response = "weight", alpha = x[1],
            P = x[2], cap = FALSE, bounds = c(0, 100), QS = c(5, 15))
      [["Data"]][103, "weight"])

  # <-><-><-><->

  expect_equal(signif(ll, 4), c(11.92, 11.90, 11.86, 11.92, 11.90, 11.87,
                                11.92, 11.91, 11.88))
  expect_equal(signif(ul, 4), c(12.10, 12.12, 12.15, 12.10, 12.11, 12.15,
                                12.09, 12.11, 12.14))

  # Results presented in SAS/QC(R) 13.1: User's Guide, Chapter 5 (The
  # CAPABILITY procedure), sub-chapter INTERVALS statement: CAPABILITY
  # procedure
  # Figure 5.25 Statistical Intervals for Weight (pp. 415-417)
  # Approximate Tolerance Interval Containing At Least Proportion p of the
  # Population (p. 416)

  # Confidence       P   Tolerance Limits
  #     99.00%   0.900        11.92 12.10
  #     99.00%   0.950        11.90 12.12
  #     99.00%   0.990        11.86 12.15
  #     95.00%   0.900        11.92 12.10
  #     95.00%   0.950        11.90 12.11
  #     95.00%   0.990        11.87 12.15
  #     90.00%   0.900        11.92 12.09
  #     90.00%   0.950        11.91 12.11
  #     90.00%   0.990        11.88 12.14
})

test_that("mztia_succeeds_with_df_shape_long", {
  tmp <-
    mztia(data = dip5, shape = "long", tcol = 1, grouping = "type",
          reference = "reference", response = "weight", alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15))[["Data"]]

  # <-><-><-><->

  expect_equal(signif(tmp[101, "weight"], 9), 12.0093000)
  expect_equal(signif(tmp[102, "weight"], 9), 11.8715203)
  expect_equal(signif(tmp[103, "weight"], 9), 12.1470797)
  expect_equal(signif(tmp[104, "weight"], 9), 6.871520286)
  expect_equal(signif(tmp[105, "weight"], 9), 17.1470797)
  expect_equal(signif(tmp[106, "weight"], 9), -3.1284797)
  expect_equal(signif(tmp[107, "weight"], 9), 27.1470797)
})

test_that("mztia_succeeds_with_df_shape_wide", {
  tmp1 <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
                reference = "R", response = NULL, alpha = 0.05, P = 0.99,
                cap = FALSE, bounds = c(0, 100), QS = c(5, 15))[["Data"]]
  tmp2 <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
                reference = "R", response = NULL, alpha = 0.05, P = 0.99,
                cap = TRUE, bounds = c(0, 100), QS = c(5, 15))[["Data"]]

  # <-><-><-><->

  expect_equal(
    signif(tmp1[tmp1$type1 == "Mean" & tmp1$type2 == "Mean", "response"], 9),
    c(46.7716667, 60.1333333, 67.2750000, 71.9866667, 78.0700000,
      84.8166667, 89.0933333, 91.4383333))
  expect_equal(
    signif(tmp1[tmp1$type1 == "TL" & tmp1$type2 == "LTL", "response"], 9),
    c(27.2264050, 46.1548274, 56.9041726, 65.4435423, 69.5425900, 77.2027513,
      76.2458801, 80.2932086))
  expect_equal(
    signif(tmp1[tmp1$type1 == "TL" & tmp1$type2 == "UTL", "response"], 9),
    c(66.3169283, 74.1118393, 77.6458274, 78.5297910, 86.5974100, 92.4305820,
      101.940787, 102.583458))
  expect_equal(
    signif(tmp1[tmp1$type1 == "TL.S1" & tmp1$type2 == "LTL", "response"], 9),
    c(22.2264050, 41.1548274, 51.9041726, 60.4435423, 64.5425900, 72.2027513,
      71.2458801, 75.2932086))
  expect_equal(
    signif(tmp1[tmp1$type1 == "TL.S1" & tmp1$type2 == "UTL", "response"], 9),
    c(71.3169283, 79.1118393, 82.6458274, 83.5297910, 91.5974100, 97.4305820,
      106.940787, 107.583458))
  expect_equal(
    signif(tmp1[tmp1$type1 == "TL.S2" & tmp1$type2 == "LTL", "response"], 9),
    c(12.2264050, 31.1548274, 41.9041726, 50.4435423, 54.5425900, 62.2027513,
      61.2458801, 65.2932086))
  expect_equal(
    signif(tmp1[tmp1$type1 == "TL.S2" & tmp1$type2 == "UTL", "response"], 9),
    c(81.3169283, 89.1118393, 92.6458274, 93.5297910, 101.5974100, 107.430582,
      116.940787, 117.583458))

  expect_equal(
    signif(tmp2[tmp2$type1 == "TL" & tmp2$type2 == "UTL", "response"], 9),
    c(66.3169283, 74.1118393, 77.6458274, 78.5297910, 86.5974100, 92.4305820,
      100.000000, 100.000000))
  expect_equal(
    signif(tmp2[tmp2$type1 == "TL.S1" & tmp2$type2 == "UTL", "response"], 9),
    c(71.3169283, 79.1118393, 82.6458274, 83.5297910, 91.5974100, 97.4305820,
      105.000000, 105.000000))
  expect_equal(
    signif(tmp2[tmp2$type1 == "TL.S2" & tmp2$type2 == "UTL", "response"], 9),
    c(81.3169283, 89.1118393, 92.6458274, 93.5297910, 101.5974100, 107.430582,
      115.000000, 115.000000))
})

test_that("mztia_fails", {
  tmp0 <- dip1
  tmp0$t.5 <- as.factor(tmp0$t.5)

  tmp1 <- dip1
  tmp1$type <-  as.character(tmp1$type)

  # <-><-><->

  expect_error(
    mztia(data = as.matrix(dip1[, 3:10]), shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "data must be provided as data frame")
  expect_error(
    mztia(data = dip1, shape = 1, tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "shape must be a string")
  expect_error(
    mztia(data = dip1, shape = "weit", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "specify shape")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = "tico",
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "tcol must be an integer")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10 + 0.1,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "tcol must be an integer")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 7:11,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "Some columns specified by tcol were not found")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 7,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "Did you provide a data frame")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 2:6,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "Some names of columns specified by tcol")
  expect_error(
    mztia(data = tmp0, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "Some columns specified by tcol are not numeric")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = 5, reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "grouping must be a string")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "lot", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "grouping variable was not found")
  expect_error(
    mztia(data = tmp1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "grouping variable's column in data")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = 1, response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "reference must be a string")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "REF", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "The reference variable was not found")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "type", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "The reference variable was not found")
  expect_error(
    mztia(data = dip5, shape = "long", tcol = 3, grouping = "type",
          reference = "reference", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "data frame provided via data is in long format"
  )
  expect_error(
    mztia(data = dip5, shape = "long", tcol = 3, grouping = "type",
          reference = "reference", response = 5, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "response must be a string of length 1"
  )
  expect_error(
    mztia(data = dip5, shape = "long", tcol = 3, grouping = "type",
          reference = "reference", response = c("type", "batch"), alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "response must be a string of length 1"
  )
  expect_error(
    mztia(data = dip5, shape = "long", tcol = 3, grouping = "type",
          reference = "reference", response = "assay", alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "response variable was not found"
  )
  expect_error(
    mztia(data = dip5, shape = "long", tcol = 3, grouping = "type",
          reference = "reference", response = "type", alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "column specified by response is not numeric"
  )
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = -1,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "specify alpha")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 9,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "specify alpha")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = -1, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "specify P")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 9, cap = FALSE, bounds = c(0, 100), QS = c(5, 15)),
    "specify P")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = 0, bounds = c(0, 100), QS = c(5, 15)),
    "cap must be a logical")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c("l", "u"), QS = c(5, 15)),
    "bounds must be a numeric vector")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(5, 50, 90), QS = c(5, 15)),
    "bounds must be a numeric vector")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(100, 0), QS = c(5, 15)),
    "specify bounds in the form")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c("l", "u")),
    "QS must be a numeric vector")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 15, 30)),
    "QS must be a numeric vector")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(-5, 15)),
    "specify QS in the range")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(5, 105)),
    "specify QS in the range")
  expect_error(
    mztia(data = dip1, shape = "wide", tcol = 3:10,
          grouping = "type", reference = "R", response = NULL, alpha = 0.05,
          P = 0.99, cap = FALSE, bounds = c(0, 100), QS = c(15, 5)),
    "Q S1 must be smaller Q S2")
})
