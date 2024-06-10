# Use of 'rand_mode = "complete"' (the default, randomise complete profiles)
# Comparison always involves only two groups.
bs1 <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                    tcol = 5:8, grouping = "batch", rand_mode = "complete",
                    rr = 200, new_seed = 421, use_ema = "no")

# Expected results in bs1[c("Boot", "BCa_CI", "ShahBCa_CI")]
# Bootstrap Statistics :
#     original      bias    std. error
# t1* 50.07187 -0.02553234   0.9488015
#
# $BCa_CI
# [1] 48.69289 51.99121
#
# $Shah_BCa_CI
# [1] 48.64613 51.75292

# Use of 'rand_mode = "individual"' (randomise per time point)
bs2 <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                    tcol = 5:8, grouping = "batch", rand_mode = "individual",
                    rr = 200, new_seed = 421, use_ema = "no")

# Expected results in bs2[c("Boot", "BCa_CI", "ShahBCa_CI")]
# Bootstrap Statistics :
#     original     bias    std. error
# t1* 50.07187 -0.1215656   0.9535517
#
# $BCa_CI
# [1] 48.88233 52.02319
#
# $Shah_BCa_CI
# [1] 48.82488 51.85736

# Passing in a data frame with a grouping variable with a number of levels that
# differs from two produces an error.
\dontrun{
  tryCatch(
    bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4", "b5"), ],
                 tcol = 5:8, grouping = "batch", rand_mode = "individual",
                 rr = 200, new_seed = 421, use_ema = "no"),
    error = function(e) message(e),
    finally = message("\nMaybe you want to remove unesed levels in data."))
}

# Error in bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4", "b5"),  :
# The number of levels in column batch differs from 2.

# Use of 'use_ema = "no"' with 'bounds = c(1, 85)'
# Since we have only 6 tablets per formulation in 'dip1' the parameter 'each'
# should be set accordingly.
bs3.1 <- bootstrap_f2(data = dip1, tcol = 3:10, grouping = "type",
                      rr = 200, each = 6, use_ema = "no", bounds = c(1, 85),
                      nsf = c(1,))

# Expected results in bs3.1[c("Boot", "BCa_CI", "ShahBCa_CI")]
# Bootstrap Statistics :
#     original     bias    std. error
# t1* 40.83405 0.06696653    1.201739
#
# $BCa_CI
# [1] 39.44222 43.88160
#
# $Shah_BCa_CI
# [1] 39.49069 43.78105

# Use of 'use_ema = "ignore"' so that the whole profiles are used (ignoring
# values passed to 'bounds')
# Since we have only 6 tablets per formulation in 'dip1' the parameter 'each'
# should be set accordingly.
bs3.2 <- bootstrap_f2(data = dip1, tcol = 3:10, grouping = "type",
                      rr = 200, each = 6, use_ema = "ignore")

# Expected results in bs3.2[c("Boot", "BCa_CI", "ShahBCa_CI")]
# Bootstrap Statistics :
#     original     bias    std. error
# t1* 42.11197 0.05937259    1.174769
#
# $BCa_CI
# [1] 40.76144 45.14164
#
# $Shah_BCa_CI
# [1] 40.82578 45.09703
