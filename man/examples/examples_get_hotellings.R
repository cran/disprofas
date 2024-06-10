# Estimation of the parameters for Hotelling's two-sample T2 statistic
# (for small samples)
\dontrun{
  res <-
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", c("t.15", "t.90")]),
                   m2 = as.matrix(dip1[dip1$type == "T", c("t.15", "t.90")]),
                   signif = 0.1)
  res$S.pool
  res$Parameters
}

# Expected results in res$S.pool
#          t.15     t.90
# t.15 3.395808 1.029870
# t.90 1.029870 4.434833

# Expected results in res$Parameters
#           DM          df1          df2       signif            K
# 1.044045e+01 2.000000e+00 9.000000e+00 1.000000e-01 1.350000e+00
#            k           T2            F       F.crit          p.F
# 3.000000e+00 3.270089e+02 1.471540e+02 3.006452e+00 1.335407e-07
