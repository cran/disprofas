# Using the defaults, only profile time points with an average release of >= 1%
# and only one time point with an average release of > 85% are taken into
# account.
res1 <- mimcr(data = dip3, tcol = 4:6, grouping = "batch")
res1$Similarity
res1$Parameters

# Expected results in res1$Similarity
#     Tsong Hoffelder
# "Similar" "Similar"

# Expected results in res1$Parameters
#            DM              df1              df2            alpha
#  2.384023e-01     3.000000e+00     2.000000e+01     5.000000e-02
#             K                k               T2                F
#  1.818182e+00     6.000000e+00     3.410141e-01     1.033376e-01
# ncp.Hoffelder           F.crit F.crit.Hoffelder              p.F
#  3.032296e+01     3.098391e+00     4.899274e+00     9.571526e-01
# p.F.Hoffelder             MTAD        Sim.Limit            Obs.L
#  2.890827e-08     1.000000e+01     2.248072e+00     1.067015e+00
#         Obs.U
#  1.543820e+00

# Comparison with T2-test for equivalence for dissolution data from the 'T2EQ'
# package
\dontrun{
  if (requireNamespace("T2EQ")) {
    library(T2EQ)
    data(ex_data_JoBS)

    T2EQ.dissolution.profiles.hoffelder(
      X = as.matrix(dip3[dip3$type == "ref", c("x.15", "x.20", "x.25")]),
      Y = as.matrix(dip3[dip3$type == "test", c("x.15", "x.20", "x.25")]))
  }

  # Excerpt of output:
  # Hotelling's T2: 			                      0.3410141
  # Noncentrality parameter:                    30.32296
  # Significance level: 		                    0.05
  # Teststatistic: 			                        0.1033376
  # Quantile of noncent. F-distribution:        4.899274
  # p-value of the T2-test for equivalence: p = 2.890827e-08
}

# Use of 'bounds = c(1, 85)'
res2 <- mimcr(data = dip1, tcol = 3:10, grouping = "type", bounds = c(1, 85),
              nsf = c(1, 2))
res2$Similarity
res2$Profile.TP
res2[["Parameters"]][c("p.F.Hoffelder", "Sim.Limit", "Obs.U")]

# Expected results in res2$Similarity
#        Tsong    Hoffelder
# "Dissimilar" "Dissimilar"

# Expected results in res2$Profile.TP
# t.5 t.10 t.15 t.20 t.30 t.60 t.90
#   5   10   15   20   30   60   90

# Expected results in res2$Parameters
# res2[["Parameters"]][c("p.F.Hoffelder", "Sim.Limit", "Obs.U")]
# p.F.Hoffelder     Sim.Limit         Obs.U
#      0.740219     11.328041     31.679020

# Allow for a larger maximum tolerable average difference (MTAD), e.g., 15.
res3 <- mimcr(data = dip1, tcol = 3:10, grouping = "type", mtad = 15,
              bounds = c(1, 85), nsf = c(1, 2))
res3$Similarity
res3[["Parameters"]][c("p.F.Hoffelder", "Sim.Limit", "Obs.U")]

# Expected results in res3$Similarity
#        Tsong    Hoffelder
# "Dissimilar" "Dissimilar"

# Expected results in res3$Parameters
# res3[["Parameters"]][c("p.F.Hoffelder", "Sim.Limit", "Obs.U")]
# p.F.Hoffelder     Sim.Limit         Obs.U
#     0.3559019    16.9920622    31.6790198

# Use default 'mtad' but set 'signif = 0.1' and use 'bounds = c(1, 95)' so that
# the complete profiles are taken into account.
res4 <- mimcr(data = dip1, tcol = 3:10, grouping = "type", mtad = 10,
              signif = 0.1, bounds = c(1, 95), nsf = c(1, 2))
res4$Similarity
res4$Profile.TP
res4[["Parameters"]][c("p.F.Hoffelder", "Sim.Limit", "Obs.U")]

# Expected results in res4$Similarity
#        Tsong    Hoffelder
# "Dissimilar" "Dissimilar"

# Expected results in res4$Profile.TP
# t.5  t.10  t.15  t.20  t.30  t.60  t.90 t.120
#   5    10    15    20    30    60    90   120

# Expected results in res4$Parameters
# res2[["Parameters"]][c("p.F.Hoffelder", "Sim.Limit", "Obs.U")]
# p.F.Hoffelder     Sim.Limit         Obs.U
#     0.1449045    19.4271898    33.3180044

\dontrun{
  # If 'max_trial' is too small, the Newton-Raphson search may not converge.
  tryCatch(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", max_trial = 5),
    warning = function(w) message(w),
    finally = message("\nMaybe increasing the number of max_trial could help."))

  # If 'tol' is too big, the points found by the Newton-Raphson search may not
  # be located on the confidence region boundary.
  tryCatch(
    mimcr(data = dip3, tcol = 4:6, grouping = "batch", tol = 1),
    warning = function(w) message(w),
    finally = message("\nMaybe making tol smaller could help."))

  # Passing in a data frame with a grouping variable with a number of levels
  # that differs from two produces an error.
  tmp <- rbind(dip1,
               data.frame(type = "T2",
                          tablet = as.factor(1:6),
                          dip1[7:12, 3:10]))

  tryCatch(
    mimcr(data = tmp, tcol = 3:10, grouping = "type", bounds = c(1, 85)),
    error = function(e) message(e),
    finally = message("\nMaybe you want to remove unesed levels in data."))

  # Error in mimcr(data = tmp, tcol = 3:10, grouping = "type", bounds = ,  :
  #   The number of levels in column type differs from 2.
}
