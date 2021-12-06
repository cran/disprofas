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

# Analyse the data by aid of the mztia() function.
res1 <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
              reference = "R", cap = FALSE)

# The 'mztia' object can be passed on to the plot_mztia() function. This
# function does not produce any output. It returns a 'plot_mztia' object that
# is essentially an 'mztia' object augmented by a 'ggplot' object.
gg1 <- plot_mztia(res1)
gg1

# Since the element gg1$Graph is a 'ggplot' object it can be used for further
# manipulation by aid of 'ggplot2' functions.
if (requireNamespace("ggplot2")) {
  library(ggplot2)

  gg1$Graph + labs(title = "Dissolution Data Assessment",
                   x = "Time [min]", y = "Drug Release [%]")
}

# Use a data frame in long format.
# Fluid weights of 100 drink cans were measured in ounces:
str(dip5)

# 'data.frame':	100 obs. of  3 variables:
# $ type  : Factor w/ 1 level "reference": 1 1 1 1 1 1 1 1 1 1 ...
# $ batch : Factor w/ 100 levels "b1","b10","b100",..: 1 13 24 35 46 57 68 ...
# $ weight: num  12.1 12 12 12 12 ...

res2 <- mztia(data = dip5, shape = "long", tcol = 3, grouping = "type",
             reference = "reference", response = "weight", cap = FALSE,
             QS = c(5, 15) / 100)

gg2 <- plot_mztia(res2)
gg2

if (requireNamespace("ggplot2")) {
  library(ggplot2)

  gg2$Graph + labs(title = "Tolerance Intervals",
                   x = NULL, y = "Weight [ounces]")
}