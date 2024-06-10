# Collecting the required information
time_points <- suppressWarnings(as.numeric(gsub("([^0-9])", "",
                                                colnames(dip1))))
tcol <- which(!is.na(time_points))
b1 <- dip1$type == "R"
tol <- 1e-9

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
                   max_trial = 100, tol = tol)

# Expected result in res[["points.on.crb"]]
# [1] NA

# Check if points lie on the confidence region bounds (CRB)
check_point_location(lpt = res, lhs = l_hs)

# Expected result in res[["points.on.crb"]]
# [1] TRUE
