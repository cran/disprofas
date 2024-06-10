# Analyse the data by aid of the mztia() function.
res1 <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
              reference = "R", cap = FALSE)

# The 'mztia' object can be passed on to the plot_mztia() function. This
# function does not produce any output. It returns a 'plot_mztia' object that
# is essentially an 'mztia' object augmented by a 'ggplot' object.
\dontrun{
  gg1 <- plot_mztia(res1)
  gg1
}

# Since the element gg1$Graph is a 'ggplot' object it can be used for further
# manipulation by aid of 'ggplot2' functions.
\dontrun{
  if (requireNamespace("ggplot2")) {
    library(ggplot2)

    gg1$Graph + labs(title = "Dissolution Data Assessment",
                     x = "Time [min]", y = "Drug Release [%]")
  }
}

# Use a data frame in long format.
res2 <- mztia(data = dip5, shape = "long", tcol = 3, grouping = "type",
             reference = "reference", response = "weight", cap = FALSE,
             QS = c(5, 15) / 100)

\dontrun{
  gg2 <- plot_mztia(res2)
  gg2

  if (requireNamespace("ggplot2")) {
    library(ggplot2)

    gg2$Graph + labs(title = "Tolerance Intervals",
                     x = NULL, y = "Weight [ounces]")
  }
}
