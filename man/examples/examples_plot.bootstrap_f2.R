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
pbs1 <- plot(bs1)

# The plot() function returns the 'plot_mztia' object invisibly.
class(bs1)
class(pbs1)

# Use of 'rand_mode = "individual"' (randomisation per time point)
bs2 <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                    tcol = 5:8, grouping = "batch", rand_mode = "individual",
                    R = 200, new_seed = 421, use_EMA = "no")
plot(bs2)