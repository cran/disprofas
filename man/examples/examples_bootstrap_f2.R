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

# Use of 'rand_mode = "complete"' (the default, randomise complete profiles)
# Comparison always involves only two groups.
bs1 <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                    tcol = 5:8, grouping = "batch", rand_mode = "complete",
                    R = 200, new_seed = 421, use_EMA = "no")

# Expected results in bs1[c("Boot", "BCa_CI", "ShahBCa_CI")]
# Bootstrap Statistics :
#     original      bias    std. error
# t1* 50.07187 -0.02553234   0.9488015
#
# $BCa_CI
# [1] 48.69289 51.99121
#
# $ShahBCa_CI
# [1] 48.64613 51.75292

# Use of 'rand_mode = "individual"' (randomise per time point)
bs2 <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                    tcol = 5:8, grouping = "batch", rand_mode = "individual",
                    R = 200, new_seed = 421, use_EMA = "no")

# Expected results in bs2[c("Boot", "BCa_CI", "ShahBCa_CI")]
# Bootstrap Statistics :
#     original     bias    std. error
# t1* 50.07187 -0.1215656   0.9535517
#
# $BCa_CI
# [1] 48.88233 52.02319
#
# $ShahBCa_CI
# [1] 48.82488 51.85736

# Passing in a data frame with a grouping variable with a number of levels that
# differs from two produces an error.
tryCatch(
  bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4", "b5"), ],
               tcol = 5:8, grouping = "batch", rand_mode = "individual",
               R = 200, new_seed = 421, use_EMA = "no"),
  error = function(e) message(e),
  finally = message("\nMaybe you want to remove unesed levels in data."))

# Error in bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4", "b5"),  :
# The number of levels in column batch differs from 2.

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

# Use of 'use_EMA = "no"' with 'bounds = c(1, 85)'
# Since we have only 6 tablets per formulation in 'dip1' the parameter 'each'
# should be set accordingly.
bs3.1 <- bootstrap_f2(data = dip1, tcol = 3:10, grouping = "type",
                      R = 200, each = 6, use_EMA = "no", bounds = c(1, 85))

# Expected results in bs3.1[c("Boot", "BCa_CI", "ShahBCa_CI")]
# Bootstrap Statistics :
#     original     bias    std. error
# t1* 40.83405 0.06696653    1.201739
#
# $BCa_CI
# [1] 39.44222 43.88160
#
# $ShahBCa_CI
# [1] 39.49069 43.78105

# Use of 'use_EMA = "ignore"' so that the whole profiles are used (ignoring
# values passed to 'bounds')
# Since we have only 6 tablets per formulation in 'dip1' the parameter 'each'
# should be set accordingly.
bs3.2 <- bootstrap_f2(data = dip1, tcol = 3:10, grouping = "type",
                      R = 200, each = 6, use_EMA = "ignore")

# Expected results in bs3.2[c("Boot", "BCa_CI", "ShahBCa_CI")]
# Bootstrap Statistics :
#     original     bias    std. error
# t1* 42.11197 0.05937259    1.174769
#
# $BCa_CI
# [1] 40.76144 45.14164
#
# $ShahBCa_CI
# [1] 40.82578 45.09703
