# Estimation of the parameters for Hotelling's two-sample T2 statistic
# (for small samples)
hs <- get_T2_two(m1 = as.matrix(dip1[dip1$type == "R", c("t.15", "t.90")]),
                 m2 = as.matrix(dip1[dip1$type == "T", c("t.15", "t.90")]),
                 signif = 0.1)

# Estimation of the similarity limit in terms of the "Multivariate Statistical
# Distance" (MSD)for a "maximum tolerable average difference" (mtad) of 10
res <- get_sim_lim(mtad = 15, hs)

# Expected results in res
#            DM              df1              df2            alpha
#  1.044045e+01     2.000000e+00     9.000000e+00     1.000000e-01
#             K                k               T2                F
#  1.350000e+00     3.000000e+00     3.270089e+02     1.471540e+02
# ncp.Hoffelder           F.crit F.crit.Hoffelder              p.F
#  2.782556e+02     3.006452e+00     8.357064e+01     1.335407e-07
# p.F.Hoffelder             MTAD        Sim.Limit
#  4.822832e-01     1.500000e+01     9.630777e+00
