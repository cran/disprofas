# Assessment of data by aid of the mimcr() function
res1 <- mimcr(data = dip1, tcol = 3:10, grouping = "type")

# Summary of the assessment
summary(res1)

# Results of Model-Independent Multivariate Confidence Region (MIMCR)
# approach to assess equivalence of highly variable in-vitro
# dissolution profiles of two drug product formulations
#
# Did the Newton-Raphson search converge? Yes
#
# Parameters (general):
# Significance level:                   0.05
# Degrees of freedom (1):               7
# Degrees of freedom (2):               4
# Mahalanobis distance (MD):            25.72
# (F) scaling factor K:                 0.1714
# (MD) scaling factor k:                3
# Hotelling's T2:                       1984
#
# Parameters specific for Tsong (1996) approach:
# Maximum tolerable average difference: 10
# Similarity limit:                     11.33
# Observed upper limit:                 31.68
#
# Parameters specific for Hoffelder (2016) approach:
# Noncentrality parameter:              385
# Critial F (Hoffelder):                23.16
# Probability p (Hoffelder):            0.7402
#
# Conclusions:
#       Tsong (1996):  Dissimilar
#   Hoffelder (2016):  Dissimilar

# Taking only the 15 and 90 minutes testing points into account produces a
# warning because profiles should comprise a minimum of three testing points.
\dontrun{
  res2 <- mimcr(data = dip1, tcol = c(5, 9), grouping = "type", mtad = 15,
                signif = 0.1)
  summary(res2)

  # Warning:
  #   In mimcr(data = dip1, tcol = c(5, 9), grouping = "type", mtad = 15,  :
  # The profiles should comprise a minimum of 3 time points. The actual profiles
  # comprise 2 points only.

  # Results of Model-Independent Multivariate Confidence Region (MIMCR)
  # approach to assess equivalence of highly variable in-vitro
  # dissolution profiles of two drug product formulations
  #
  # Did the Newton-Raphson search converge? Yes
  #
  # Parameters (general):
  # Significance level:                   0.1
  # Degrees of freedom (1):               2
  # Degrees of freedom (2):               9
  # Mahalanobis distance (MD):            10.44
  # (F) scaling factor K:                 1.35
  # (MD) scaling factor k:                3
  # Hotelling's T2:                       327
  #
  # Parameters specific for Tsong (1996) approach:
  # Maximum tolerable average difference: 15
  # Similarity limit:                     9.631
  # Observed upper limit:                 11.93
  #
  # Parameters specific for Hoffelder (2016) approach:
  # Noncentrality parameter:              278.3
  # Critial F (Hoffelder):                83.57
  # Probability p (Hoffelder):            0.4823
  #
  # Conclusions:
  #       Tsong (1996):  Dissimilar
  #   Hoffelder (2016):  Dissimilar
}

# A successful comparison:
res3 <- mimcr(data = dip3, tcol = 4:6, grouping = "batch")
summary(res3)

# Results of Model-Independent Multivariate Confidence Region (MIMCR)
# approach to assess equivalence of highly variable in-vitro
# dissolution profiles of two drug product formulations
#
# Did the Newton-Raphson search converge? Yes
#
# Parameters (general):
#   Significance level:                 0.05
# Degrees of freedom (1):               3
# Degrees of freedom (2):               20
# Mahalanobis distance (MD):            0.2384
# (F) scaling factor K:                 1.818
# (MD) scaling factor k:                6
# Hotelling's T2:                       0.341
#
# Parameters specific for Tsong (1996) approach:
# Maximum tolerable average difference: 10
# Similarity limit:                     2.248
# Observed upper limit:                 1.544
#
# Parameters specific for Hoffelder (2016) approach:
# Noncentrality parameter:              30.32
# Critial F (Hoffelder):                4.899
# Probability p (Hoffelder):            2.891e-08
#
# Conclusions:
#       Tsong (1996):  Similar
#   Hoffelder (2016):  Similar
