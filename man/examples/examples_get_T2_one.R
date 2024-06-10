# Estimation of the parameters for Hotelling's one-sample T2 statistic
# (for small samples)
# Check if there is a significant difference of the test batch results
# from the average reference batch results.
# Since p.F in res1$Parameters is smaller than 0.1, it is concluded that the
# new batch differs from the reference batch.
res1 <-
  get_T2_one(m = as.matrix(dip1[dip1$type == "T", c("t.15", "t.90")]),
             mu = colMeans(dip1[dip1$type == "R", c("t.15", "t.90")]),
             signif = 0.1, na_rm = FALSE)
res1$Parameters

# Expected results in res1$Parameters
#           dm          df1          df2       signif            K
# 1.314197e+01 2.000000e+00 4.000000e+00 1.000000e-01 2.400000e+00
#            k           T2            F       F.crit       t.crit
# 6.000000e+00 1.036268e+03 4.145072e+02 4.324555e+00 2.570582e+00
#          p.F
# 2.305765e-05

# In Tsong (1997) (see reference of dip7), the model-dependent approach is
# illustrated with an example data set of alpha and beta parameters obtained
# by fitting the Weibull curve function to a data set of dissolution profiles
# of three reference batches and one new batch (12 profiles per batch).
# Check if there is a significant difference of the test batch results
# from the average reference batch results.
# Since p.F in res2$Parameters is smaller than 0.05, it is concluded that the
# test batch differs from the reference batches.
res2 <-
  get_T2_one(m = as.matrix(dip7[dip7$type == "test", c("alpha", "beta")]),
             mu = colMeans(dip7[dip7$type == "ref", c("alpha", "beta")]),
             signif = 0.05, na_rm = FALSE)
res2$Parameters

# Expected results in res2$Parameters
#           dm          df1          df2       signif            K
# 5.984856e+00 2.000000e+00 1.000000e+01 5.000000e-02 5.454545e+00
#            k           T2            F       F.crit       t.crit
# 1.200000e+01 4.298220e+02 1.953736e+02 4.102821e+00 2.593093e+00
#          p.F
# 9.674913e-09

# In Sathe (1996) (see reference of dip8), the model-dependent approach is
# illustrated with an example data set of alpha and beta parameters obtained
# by fitting the Weibull curve function to a data set of dissolution profiles
# of one reference batch and one new batch with minor modifications and another
# new batch with major modifications (12 profiles per batch).
# Check if there is a significant difference of the results of the minor or
# major modificated batches from the average reference batch results.
# Since p.F in res3.minor$Parameters or in res3.major$Parameters are smaller
# than 0.1, it is concluded that the minor and the major modification batch
# differs from the reference batch.
res3.minor <-
  get_T2_one(m = log(as.matrix(dip8[dip8$type == "minor",
                                    c("alpha", "beta")])),
             mu = log(colMeans(dip8[dip8$type == "ref",
                                     c("alpha", "beta")])),
             signif = 0.1, na_rm = FALSE)
res3.major <-
  get_T2_one(m = log(as.matrix(dip8[dip8$type == "major",
                                    c("alpha", "beta")])),
             mu = log(colMeans(dip8[dip8$type == "ref",
                                     c("alpha", "beta")])),
             signif = 0.1, na_rm = FALSE)
res3.minor$Parameters
res3.major$Parameters

# Expected results in res3.minor$Parameters
#           dm          df1          df2       signif            K
# 2.718715e+00 2.000000e+00 1.000000e+01 1.000000e-01 5.454545e+00
#            k           T2            F       F.crit       t.crit
# 1.200000e+01 8.869691e+01 4.031678e+01 2.924466e+00 2.200985e+00
#          p.F
# 1.635140e-05

# Expected results in res3.major$Parameters
#           dm          df1          df2       signif            K
# 5.297092e+00 2.000000e+00 1.000000e+01 1.000000e-01 5.454545e+00
#            k           T2            F       F.crit       t.crit
# 1.200000e+01 3.367102e+02 1.530501e+02 2.924466e+00 2.200985e+00
#          p.F
# 3.168664e-08
