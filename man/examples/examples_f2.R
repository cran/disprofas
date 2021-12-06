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

# Use of defaults, i.e. 'use_EMA = "yes"', 'bounds = c(1, 85)'
# Comparison always involves only two groups.
f2(data = dip1, tcol = 3:10, grouping = "type")

# $f2
# [1] 40.83405
#
# $Profile.TP
# t.5 t.10 t.15 t.20 t.30 t.60 t.90
#   5   10   15   20   30   60   90

# Use of 'use_EMA = "no"', 'bounds = c(5, 80)'
f2(data = dip1, tcol = 3:10, grouping = "type", use_EMA = "no",
   bounds = c(5, 80))

# $f2
# [1] 39.24385
#
# $Profile.TP
# t.5 t.10 t.15 t.20 t.30 t.60
#   5   10   15   20   30   60

# Use of 'use_EMA = "no"', 'bounds = c(1, 95)'
f2(data = dip1, tcol = 3:10, grouping = "type", use_EMA = "no",
   bounds = c(1, 95))

# $f2
# [1] 42.11197
#
# $Profile.TP
# t.5  t.10  t.15  t.20  t.30  t.60  t.90 t.120
#   5    10    15    20    30    60    90   120

# In this case, the whole profiles are used. The same result is obtained
# when setting 'use_EMA = "ignore"' (ignoring values passed to 'bounds').
f2(data = dip1, tcol = 3:10, grouping = "type", use_EMA = "ignore")

# Passing in a data frame with a grouping variable with a number of levels that
# differs from two produces an error.
tmp <- rbind(dip1,
             data.frame(type = "T2",
                        tablet = as.factor(1:6),
                        dip1[7:12, 3:10]))

tryCatch(
  f2(data = tmp, tcol = 3:10, grouping = "type"),
  error = function(e) message(e),
  finally = message("\nMaybe you want to remove unesed levels in data."))

# Error in f1(data = tmp, tcol = 3:10, grouping = "type") :
#   The number of levels in column type differs from 2.
