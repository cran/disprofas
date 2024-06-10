# Bootstrap assessment of data (two groups) by aid of bootstrap_f2() function
# by using 'rand_mode = "complete"' (the default, randomisation of complete
# profiles)
bs1 <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                    tcol = 5:8, grouping = "batch", rand_mode = "complete",
                    rr = 200, new_seed = 421, use_ema = "no")

\dontrun{
  pbs1 <- plot(bs1)

  # The plot() function returns the 'plot_mztia' object invisibly.
  class(bs1)
  class(pbs1)
}

# Use of 'rand_mode = "individual"' (randomisation per time point)
bs2 <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                    tcol = 5:8, grouping = "batch", rand_mode = "individual",
                    rr = 200, new_seed = 421, use_ema = "no")

\dontrun{
  plot(bs2)
}
