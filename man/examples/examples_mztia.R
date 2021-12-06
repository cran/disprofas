# Fluid weights of 100 drink cans were measured in ounces:
str(dip5)

# 'data.frame':	100 obs. of  3 variables:
#   $ type  : Factor w/ 1 level "reference": 1 1 1 1 1 1 1 1 1 1 ...
# $ batch : Factor w/ 100 levels "b1","b10","b100",..: 1 13 24 35 46 57 68 ...
# $ weight: num  12.1 12 12 12 12 ...

# Calculation of tolerance intervals
m_alpha_P <- matrix(c(rep(c(0.01, 0.05, 0.1), each = 3),
                      1 - rep(c(0.1, 0.05, 0.01), times = 3)),
                    ncol = 2, byrow = FALSE)

ll <-
  apply(m_alpha_P, MARGIN = 1, FUN = function(x)
    mztia(data = dip5, shape = "long", tcol = 1, grouping = "type",
          reference = "reference", response = "weight", alpha = x[1],
          P = x[2], cap = FALSE)[["Data"]][102, "weight"])
ul <-
  apply(m_alpha_P, MARGIN = 1, FUN = function(x)
    mztia(data = dip5, shape = "long", tcol = 1, grouping = "type",
          reference = "reference", response = "weight", alpha = x[1],
          P = x[2], cap = FALSE)[["Data"]][103, "weight"])

# Expected results in ll and ul
rbind(ll, ul)
#        [,1]    [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]
# ll 11.91648 11.8987 11.86395 11.92132 11.90446 11.87152 11.92373 11.90734
# ul 12.10212 12.1199 12.15465 12.09728 12.11414 12.14708 12.09487 12.11126
#       [,9]
# ll 11.8753
# ul 12.1433

# Use a data frame in wide format
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

# Using the defaults; Limits are capped to the range specified by 'bounds'
res1 <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
              reference = "R")
res1$Limits

# Expected results in res1$Limits
# Time     Mean      LTL       UTL   S1.LTL    S1.UTL   S2.LTL    S2.UTL
# 1    5 46.77167 27.22641  66.31693 22.22641  71.31693 12.22641  81.31693
# 2   10 60.13333 46.15483  74.11184 41.15483  79.11184 31.15483  89.11184
# 3   15 67.27500 56.90417  77.64583 51.90417  82.64583 41.90417  92.64583
# 4   20 71.98667 65.44354  78.52979 60.44354  83.52979 50.44354  93.52979
# 5   30 78.07000 69.54259  86.59741 64.54259  91.59741 54.54259 101.59741
# 6   60 84.81667 77.20275  92.43058 72.20275  97.43058 62.20275 107.43058
# 7   90 89.09333 76.24588 100.00000 71.24588 105.00000 61.24588 115.00000
# 8  120 91.43833 80.29321 100.00000 75.29321 105.00000 65.29321 115.00000

# Without capping of limits to 105%
res2 <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
              reference = "R", cap = FALSE)
res2$Limits

# Expected results in res1$Limits
# Time     Mean      LTL       UTL   S1.LTL    S1.UTL   S2.LTL    S2.UTL
# 1    5 46.77167 27.22641  66.31693 22.22641  71.31693 12.22641  81.31693
# 2   10 60.13333 46.15483  74.11184 41.15483  79.11184 31.15483  89.11184
# 3   15 67.27500 56.90417  77.64583 51.90417  82.64583 41.90417  92.64583
# 4   20 71.98667 65.44354  78.52979 60.44354  83.52979 50.44354  93.52979
# 5   30 78.07000 69.54259  86.59741 64.54259  91.59741 54.54259 101.59741
# 6   60 84.81667 77.20275  92.43058 72.20275  97.43058 62.20275 107.43058
# 7   90 89.09333 76.24588 101.94079 71.24588 106.94079 61.24588 116.94079
# 8  120 91.43833 80.29321 102.58346 75.29321 107.58346 65.29321 117.58346

# Tolerance intervals are calculated exclusively for the level of the
# grouping variable that is specified by the reference variable. Therefore,
# the following code produces the same limits summary as in res2$Limits.
tmp <- rbind(dip1,
             data.frame(type = "T2",
                        tablet = as.factor(1:6),
                        dip1[7:12, 3:10]))

res2 <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
              reference = "R", cap = FALSE)
res3 <- mztia(data = tmp, shape = "wide", tcol = 3:10, grouping = "type",
              reference = "R", cap = FALSE)

isTRUE(all.equal(res2$Limits, res3$Limits))
# [1] TRUE
