# Dissolution data of one reference batch and one test batch of n = 6
# tablets each:
str(dip1)

# 'data.frame':	12 obs. of  10 variables:
# $ type  : Factor w/ 2 levels "R","T": 1 1 1 1 1 1 2 2 2 2 ...
# $ tablet: Factor w/ 6 levels "1","2","3","4",..: 1 2 3 4 5 6 1 2 3 4 ...
# $ t.5   : num  42.1 44.2 45.6 48.5 50.5 ...
# $ t.10  : num  59.9 60.2 55.8 60.4 61.8 ...
# $ t.15  : num  65.6 67.2 65.6 66.5 69.1 ...
# $ t.20  : num  71.8 70.8 70.5 73.1 72.8 ...
# $ t.30  : num  77.8 76.1 76.9 78.5 79 ...
# $ t.60  : num  85.7 83.3 83.9 85 86.9 ...
# $ t.90  : num  93.1 88 86.8 88 89.7 ...
# $ t.120 : num  94.2 89.6 90.1 93.4 90.8 ...

# Estimation of the parameters for Hotelling's two-sample T2 statistic
# (for small samples)
res <-
  get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", c("t.15", "t.90")]),
                 m2 = as.matrix(dip1[dip1$type == "T", c("t.15", "t.90")]),
                 signif = 0.1)
res$S.pool
res$Parameters

# Expected results in res$S.pool
#          t.15     t.90
# t.15 3.395808 1.029870
# t.90 1.029870 4.434833

# Expected results in res$Parameters
#           DM          df1          df2       signif            K
# 1.044045e+01 2.000000e+00 9.000000e+00 1.000000e-01 1.350000e+00
#            k           T2            F       F.crit          p.F
# 3.000000e+00 3.270089e+02 1.471540e+02 3.006452e+00 1.335407e-07
