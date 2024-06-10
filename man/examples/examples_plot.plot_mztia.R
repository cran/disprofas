# Assessment of data by aid of the mztia() function
res1 <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
              reference = "R", cap = FALSE)

# The 'mztia' object can be passed on to the plot_mztia() function. This
# function does not produce any output but returns a 'plot_mztia' object.
\dontrun{
  gg1 <- plot_mztia(res1)
  gg2 <- plot(gg1)

  # The plot() function returns the 'plot_mztia' object invisibly.
  class(gg1)
  class(gg2)
}
