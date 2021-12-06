# Dissolution data of one reference batch and five test batches of n = 12
# tablets each:
str(dip2)

# 'data.frame':	72 obs. of  8 variables:
# $ type  : Factor w/ 2 levels "Reference","Test": 1 1 1 1 1 1 1 1 1 1 ...
# $ tablet: Factor w/ 12 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ batch : Factor w/ 6 levels "b0","b1","b2",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ t.0   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ t.30  : num  36.1 33 35.7 32.1 36.1 34.1 32.4 39.6 34.5 38 ...
# $ t.60  : num  58.6 59.5 62.3 62.3 53.6 63.2 61.3 61.8 58 59.2 ...
# $ t.90  : num  80 80.8 83 81.3 72.6 83 80 80.4 76.9 79.3 ...
# $ t.180 : num  93.3 95.7 97.1 92.8 88.8 97.4 96.8 98.6 93.3 94 ...

# Bootstrap assessment of data (two groups) by aid of bootstrap_f2() function
# by using 'rand_mode = "complete"' (the default, randomisation of complete
# profiles)
bs1 <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                    tcol = 5:8, grouping = "batch", rand_mode = "complete",
                    R = 200, new_seed = 421, use_EMA = "no")

# Summary of the assessment
summary(bs1)

# STRATIFIED BOOTSTRAP
#
#
# Call:
#   boot(data = data, statistic = get_f2, R = R, strata = data[, grouping],
#        grouping = grouping, tcol = tcol[ok])
#
#
# Bootstrap Statistics :
#   original      bias    std. error
# t1* 50.07187 -0.02553234   0.9488015
#
#
# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 200 bootstrap replicates
#
# CALL :
#   boot.ci(boot.out = t_boot, conf = confid, type = "all", L = jack$loo.values)
#
# Intervals :
#   Level      Normal              Basic
# 90%   (48.54, 51.66 )   (48.46, 51.71 )
#
# Level     Percentile            BCa
# 90%   (48.43, 51.68 )   (48.69, 51.99 )
# Calculations and Intervals on Original Scale
# Some BCa intervals may be unstable
#
#
# Shah's lower 90% BCa confidence interval:
#  48.64613

# Use of 'rand_mode = "individual"' (randomisation per time point)
bs2 <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                    tcol = 5:8, grouping = "batch", rand_mode = "individual",
                    R = 200, new_seed = 421, use_EMA = "no")

# Summary of the assessment
summary(bs2)

# PARAMETRIC BOOTSTRAP
#
#
# Call:
#   boot(data = data, statistic = get_f2, R = R, sim = "parametric",
#        ran.gen = rand_indiv_points, mle = mle, grouping = grouping,
#        tcol = tcol[ok], ins = seq_along(b1))
#
#
# Bootstrap Statistics :
#   original     bias    std. error
# t1* 50.07187 -0.1215656   0.9535517
#
#
# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 200 bootstrap replicates
#
# CALL :
#   boot.ci(boot.out = t_boot, conf = confid, type = "all", L = jack$loo.values)
#
# Intervals :
#   Level      Normal              Basic
# 90%   (48.62, 51.76 )   (48.44, 51.64 )
#
# Level     Percentile            BCa
# 90%   (48.50, 51.70 )   (48.88, 52.02 )
# Calculations and Intervals on Original Scale
# Some BCa intervals may be unstable
#
#
# Shah's lower 90% BCa confidence interval:
#  48.82488