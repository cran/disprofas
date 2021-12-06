# Collecting the required information
time_points <- suppressWarnings(as.numeric(gsub("([^0-9])", "",
                                                colnames(dip1))))
tico <- which(!is.na(time_points))
b1 <- dip1$type == "R"

n_tp <- length(tico)
n_b1 <- length(dip1[b1, "type"])
n_b2 <- length(dip1[!b1, "type"])
df_b1 <- n_tp
df_b2 <- n_b1 + n_b2 - n_tp - 1
K_limit <- (n_b2 * n_b1) / (n_b2 + n_b1)
K <- K_limit * df_b2 / (df_b1 * (n_b2 + n_b1 - 2))

mean_b1 <- apply(X = dip1[b1, tico], MARGIN = 2, FUN = mean)
mean_b2 <- apply(X = dip1[!b1, tico], MARGIN = 2, FUN = mean)
mean_diff <- mean_b2 - mean_b1

S_b1 <- cov(dip1[b1, tico])
S_b2 <- cov(dip1[!b1, tico])
S <- ((n_b1 - 1) * S_b1 + (n_b2 - 1) * S_b2) / (n_b1 + n_b2 - 2)

F_crit <- qf(p = (1 - 0.05), df1 = df_b1, df2 = df_b2)
y_b1 <- rep(1, times = (n_tp + 1))

# Calling gep_by_nera()
res <- gep_by_nera(n_p = n_tp, K = K, mean_diff = mean_diff, S_pool = S,
                   F_crit = F_crit, y = y_b1, max_trial = 100, tol = 1e-9)

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
tryCatch(
  gep_by_nera(n_p = n_tp, K = K, mean_diff = mean_diff, S_pool = S,
              F_crit = F_crit, y = y_b1, max_trial = 5, tol = 1e-9),
  warning = function(w) message(w),
  finally = message("\nMaybe increasing the number of max_trial could help."))
