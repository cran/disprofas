# Use of defaults, i.e. 'use_ema = "yes"', 'bounds = c(1, 85)'
# Comparison always involves only two groups.
f1(data = dip1, tcol = 3:10, grouping = "type")

# $f1
# [1] 18.19745
#
# $Profile.TP
# t.5 t.10 t.15 t.20 t.30 t.60 t.90
#   5   10   15   20   30   60   90

# Use of 'use_ema = "no"', 'bounds = c(5, 80)'
f1(data = dip1, tcol = 3:10, grouping = "type", use_ema = "no",
   bounds = c(5, 80), nsf = c(1, 2))

# $f1
# [1] 21.333
#
# $Profile.TP
# t.5 t.10 t.15 t.20 t.30 t.60
#   5   10   15   20   30   60

# Use of 'use_ema = "no"', 'bounds = c(1, 95)'
f1(data = dip1, tcol = 3:10, grouping = "type", use_ema = "no",
   bounds = c(1, 95), nsf = c(1, 2))

# $f1
# [1] 16.22299
#
# $Profile.TP
# t.5  t.10  t.15  t.20  t.30  t.60  t.90 t.120
#   5    10    15    20    30    60    90   120

# In this case, the whole profiles are used. The same result is obtained
# when setting 'use_ema = "ignore"' (ignoring values passed to 'bounds').
f1(data = dip1, tcol = 3:10, grouping = "type", use_ema = "ignore")

# Passing in a data frame with a grouping variable with a number of levels that
# differs from two produces an error.
\dontrun{
  tmp <- rbind(dip1,
               data.frame(type = "T2",
                          tablet = as.factor(1:6),
                          dip1[7:12, 3:10]))

  tryCatch(
    f1(data = tmp, tcol = 3:10, grouping = "type"),
    error = function(e) message(e),
    finally = message("\nMaybe you want to remove unesed levels in data."))
}

# Error in f1(data = tmp, tcol = 3:10, grouping = "type") :
#   The number of levels in column type differs from 2.
