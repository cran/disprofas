# Bootstrap assessment of data (two groups) by aid of bootstrap_f2() function
# by using 'rand_mode = "complete"' (the default, randomisation of complete
# profiles)
bs1 <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                    tcol = 5:8, grouping = "batch", rand_mode = "complete",
                    rr = 200, new_seed = 421, use_ema = "no")

# Print of a summary of the assessment
print(bs1)

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
                    rr = 200, new_seed = 421, use_ema = "no")

# Print of a summary of the assessment
print(bs2)

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
