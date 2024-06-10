# Collecting the required information
time_points <- suppressWarnings(as.numeric(gsub("([^0-9])", "",
                                                colnames(dip1))))
tcol <- which(!is.na(time_points))
b1 <- dip1$type == "R"

# Hotelling's T2 statistics
l_hs <- get_T2_two(m1 = as.matrix(dip1[b1, tcol]),
                   m2 = as.matrix(dip1[!b1, tcol]),
                   signif = 0.05)

# Calling gep_by_nera()
res <- gep_by_nera(n_p = as.numeric(l_hs[["Parameters"]]["df1"]),
                   kk = as.numeric(l_hs[["Parameters"]]["K"]),
                   mean_diff = l_hs[["means"]][["mean.diff"]],
                   m_vc = l_hs[["S.pool"]],
                   ff_crit = as.numeric(l_hs[["Parameters"]]["F.crit"]),
                   y = rep(1, times = l_hs[["Parameters"]]["df1"] + 1),
                   max_trial = 100, tol = 1e-9)

# Expected result in res[["points"]]
#              [,1]
# t.5   -15.7600077
# t.10  -13.6501734
# t.15  -11.6689469
# t.20   -9.8429369
# t.30   -6.6632182
# t.60   -0.4634318
# t.90    2.2528551
# t.120   3.3249569
#       -17.6619995

# Rows t.5 to t.120 represent the points on the CR bounds.The unnamed last row
# represents the Lagrange multiplier lambda.

# If 'max_trial' is too small, the Newton-Raphson search may not converge.
\dontrun{
  tryCatch(
    gep_by_nera(n_p = as.numeric(l_hs[["Parameters"]]["df1"]),
                kk = as.numeric(l_hs[["Parameters"]]["K"]),
                mean_diff = l_hs[["means"]][["mean.diff"]],
                m_vc = l_hs[["S.pool"]],
                ff_crit = as.numeric(l_hs[["Parameters"]]["F.crit"]),
                y = rep(1, times = l_hs[["Parameters"]]["df1"] + 1),
                max_trial = 5, tol = 1e-9),
    warning = function(w) message(w),
    finally = message("\nMaybe increasing the number of max_trial could help."))
}
