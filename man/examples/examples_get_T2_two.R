# Estimation of the parameters for Hotelling's two-sample T2 statistic
# (for small samples)
res1 <- get_T2_two(m1 = as.matrix(dip1[dip1$type == "R", c("t.15", "t.90")]),
                   m2 = as.matrix(dip1[dip1$type == "T", c("t.15", "t.90")]),
                   signif = 0.1)
res1$S.pool
res1$Parameters

# Results in res1$S.pool
#          t.15     t.90
# t.15 3.395808 1.029870
# t.90 1.029870 4.434833

# Results in res1$Parameters
#           dm          df1          df2       signif            K
# 1.044045e+01 2.000000e+00 9.000000e+00 1.000000e-01 1.350000e+00
#            k           T2            F       F.crit       t.crit
# 3.000000e+00 3.270089e+02 1.471540e+02 3.006452e+00 2.228139e+00
#          p.F
# 1.335407e-07

# The results above correspond to the values that are shown in Tsong (1996)
# (see reference of dip1 data set) under paragraph "DATA1 data (Comparing
# the 15- and 90-minute sample time points only).

# For the second assessment shown in Tsong (1996) (see reference of dip1 data
# set) under paragraph "DATA2 data (Comparing all eight time points), the
# following results are obtained.
res2 <- get_T2_two(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                   m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                   signif = 0.1)
res2$Parameters

# Results in res2$Parameters
#           dm          df1          df2       signif            K
# 2.648562e+01 8.000000e+00 3.000000e+00 1.000000e-01 1.125000e-01
#            k           T2            F       F.crit       t.crit
# 3.000000e+00 2.104464e+03 7.891739e+01 5.251671e+00 3.038243e+00
#          p.F
# 2.116258e-03

# In Tsong (1997) (see reference of dip7), the model-dependent approach is
# illustrated with an example data set of alpha and beta parameters obtained
# by fitting the Weibull curve function to a data set of dissolution profiles
# of three reference batches and one new batch (12 profiles per batch).
res3 <-
  get_T2_two(m1 = as.matrix(dip7[dip7$type == "ref", c("alpha", "beta")]),
             m2 = as.matrix(dip7[dip7$type == "test", c("alpha", "beta")]),
             signif = 0.05)
res3$Parameters

# Results in res3$Parameters
#           dm          df1          df2       signif            K
# 3.247275e+00 2.000000e+00 4.500000e+01 5.000000e-02 4.402174e+00
#            k           T2            F       F.crit       t.crit
# 9.000000e+00 9.490313e+01 4.642001e+01 3.204317e+00 2.317152e+00
#          p.F
# 1.151701e-11

# In Sathe (1996) (see reference of dip8), the model-dependent approach is
# illustrated with an example data set of alpha and beta parameters obtained
# by fitting the Weibull curve function to a data set of dissolution profiles
# of one reference batch and one new batch with minor modifications and another
# new batch with major modifications (12 profiles per batch). Note that the
# assessment is performed on the (natural) logarithm scale.
res4.minor <- get_T2_two(m1 = log(as.matrix(dip8[dip8$type == "ref",
                                                 c("alpha", "beta")])),
                         m2 = log(as.matrix(dip8[dip8$type == "minor",
                                                 c("alpha", "beta")])),
                         signif = 0.1)
res4.major <- get_T2_two(m1 = log(as.matrix(dip8[dip8$type == "ref",
                                                 c("alpha", "beta")])),
                         m2 = log(as.matrix(dip8[dip8$type == "major",
                                                 c("alpha", "beta")])),
                         signif = 0.1)
res4.minor$Parameters
res4.minor$CI$Hotelling
res4.major$Parameters
res4.major$CI$Hotelling

# Expected results in res4.minor$Parameters
#          dm          df1          df2       signif            K
# 1.462603730  2.000000000 21.000000000  0.100000000  2.863636364
#           k           T2            F       F.crit       t.crit
# 6.000000000 12.835258028  6.125918604  2.574569390  2.073873068
#         p.F
# 0.008021181

# Results in res4.minor$CI$Hotelling
#              LCL         UCL
# alpha -0.2553037 -0.02814098
# beta  -0.1190028  0.01175691

# Expected results in res4.major$Parameters
#           dm          df1          df2       signif            K
# 4.508190e+00 2.000000e+00 2.100000e+01 5.000000e-02 2.863636e+00
#            k           T2            F       F.crit       t.crit
# 6.000000e+00 1.219427e+02 5.819992e+01 2.574569e+00 2.073873e+00
#          p.F
# 2.719240e-09

# Expected results in res4.major$CI$Hotelling
#              LCL        UCL
# alpha -0.4864736 -0.2360966
# beta   0.1954760  0.3035340
