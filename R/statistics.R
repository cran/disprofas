#' Hotelling's statistics (for one (small) sample)
#'
#' The function \code{get_T2_one()} estimates the parameters for Hotelling's
#' one-sample \eqn{T^2} statistic for small samples.
#'
#' @param m A matrix with the data of the reference group, e.g. a matrix
#'   for the different model parameters (columns) of different dosage unit
#'   (rows).
#' @param mu A numeric vector of, e.g. the hypothetical model parameter
#'   mean values.
#' @param signif A positive numeric value between \code{0} and \code{1}
#'   that specifies the significance level. The default value is \code{0.05}.
#' @param na_rm A logical value that indicates whether observations containing
#'   \code{NA} (or \code{NaN}) values should be removed (\code{na_rm = TRUE})
#'   or not (\code{na_rm = FALSE}). The default is \code{na_rm = FALSE}.
#'
#' @details The one-sample Hotelling's \eqn{T^2} test statistic is given by
#'
#' \deqn{T^2 = n \left( \bar{\bm{x}} - \bm{\mu}_0 \right)^{\top}
#'       \bm{S}^{-1} \left( \bar{\bm{x}} - \bm{\mu}_0 \right) .}{%
#'       T^2 = n (x.bar - \mu_0)^{\top} S^{-1} (x.bar - \mu_0) .}
#'
#' where \eqn{\bar{\bm{x}}}{x.bar} is the vector of the sample means of the
#' sample group, e.g. the vector of the average dissolution per time point or
#' of the average model parameters, \eqn{n} is the numbers of observations of
#' the sample group (i.e. the number of rows in matrix \code{m} handed over
#' to the \code{get_T2_one()} function, and \eqn{\bm{S}} is variance-covariance
#' matrix. The matrix \eqn{\bm{S}^{-1}}{S^{-1}} is the inverted
#' variance-covariance matrix. The term
#'
#' \deqn{D_M = \sqrt{ \left( \bar{\bm{x}} - \bm{\mu}_0 \right)^{\top}
#'       \bm{S}^{-1} \left( \bar{\bm{x}} - \bm{\mu}_0 \right) }}{%
#'   D_M = sqrt((x.bar - \mu_0)^{\top} S^{-1} (x.bar - \mu_0))}
#'
#' is the Mahalanobis distance measuring the difference between the sample mean
#' vector and the vector of the hypothetical values \eqn{\bm{\mu}_0}{\mu_0}.
#' For large samples, \eqn{T^2} is approximately chi-square distributed with
#' \eqn{p} degrees of freedom, where \eqn{p} is the number of variables, i.e.
#' the number of dissolution profile time points or the number of model
#' parameters. In terms of the Mahalanobis distance, the one-sample Hotelling's
#' \eqn{T^2} statistic can be expressed has
#'
#' \deqn{n \; D_M^2 = k \; D_M^2 .}
#'
#' To transform the one-sample Hotelling's \eqn{T^2} statistic into an
#' \eqn{F}-statistic, a conversion factor is necessary, i.e.
#'
#' \deqn{K = k \; \frac{n - p}{(n - 1) p} .}{k (n - p) / ((n - 1) p) .}
#'
#' With this transformation, the following test statistic can be applied:
#'
#' \deqn{K \; D_M^2 \leq F_{p, n - p, \alpha} .}{%
#'   K D_M^2 \leq F_{p, n - p, \alpha} .}
#'
#' Under the null hypothesis, \eqn{H_0: \bm{\mu} = \bm{\mu}_0}{%
#' H_0: \mu = \mu_0}, this \eqn{F}-statistic is \eqn{F}-distributed with
#' \eqn{p} and \eqn{n - p} degrees of freedom. \eqn{H_0} is rejected at a
#' significance level of \eqn{\alpha} if the test statistic \eqn{F} exceeds
#' the critical value from the \eqn{F}-table evaluated at \eqn{\alpha}, i.e.
#' \eqn{F > F_{p, n - p, \alpha}}. \cr
#'
#' The following assumptions concerning the data are made:
#' \itemize{
#' \item The data of population \eqn{x} has no sub-populations, i.e. there are
#'   no sub-populations of \eqn{x} with different means.
#' \item The observations are based on a common variance-covariance matrix
#'   \eqn{\Sigma}.
#' \item The observations have been independently sampled.
#' \item The observations have been sampled from a multivariate normal
#'   distribution.
#' }
#'
#' \strong{Confidence intervals}: \cr
#' Simultaneous \eqn{(1 - \alpha)100\%} confidence intervals for all linear
#' combinations of the sample means are given by the expression
#'
#' \deqn{\left( \bar{\bm{x}} - \bm{\mu}_0 \right) \pm
#' \sqrt{\frac{1}{K} \; F_{p, n - p, \alpha} \; \bm{s}} ,}{%
#'   (x.bar - \mu_0) \pm sqrt(1 / K F_{p, n - p, \alpha} s) ,}
#'
#' where \eqn{\bm{s}}{s} is the vector of the diagonal elements of the
#' variance-covariance matrix \eqn{\bm{S}}{S}. With \eqn{(1 - \alpha)100\%}
#' confidence, this interval covers the respective linear combination of the
#' differences between the sample means and the hypothetical means. If not
#' the linear combination of the variables is of interest but rather the
#' individual variables, then the Bonferroni corrected confidence intervals
#' should be used instead which are given by the expression
#'
#' \deqn{\left( \bar{\bm{x}} - \bm{\mu}_0 \right) \pm
#'   t_{n - 1, \frac{\alpha}{2 p}} \;
#'   \sqrt{\frac{1}{k} \; \bm{s}} .}{%
#'   (x_T - x_R) \pm t_{n - 1, \alpha / (2 p)} sqrt(1 / k s) .}
#'
#' @return A list with the following elements is returned:
#' \item{Parameters}{Parameters determined for the estimation of Hotelling's
#'   \eqn{T^2}.}
#' \item{cov}{The variance-covariance matrix of the reference group.}
#' \item{means}{A list with the elements \code{mean.r}, \code{mean.t} and
#'   \code{mean.diff}, i.e. the average model parameters of the reference
#'   group, the hypothetical average model parameters (handed over via the
#'   \code{mu} parameter) and the corresponding differences, respectively.}
#' \item{CI}{A list with the elements \code{Hotelling} and \code{Bonferroni},
#'   i.e. data frames with columns \code{LCL} and \code{UCL} for the lower
#'   and upper \eqn{(1 - \alpha)100\%} confidence limits, respectively, and
#'   rows for each time point or model parameter.}
#'
#' The \code{Parameters} element contains the following information:
#' \item{dm}{Mahalanobis distance of the samples.}
#' \item{df1}{Degrees of freedom (number of variables or time points).}
#' \item{df2}{Degrees of freedom (number of rows - number of variables - 1).}
#' \item{alpha}{Provided significance level.}
#' \item{K}{Scaling factor for \eqn{F} to account for the distribution of the
#'   \eqn{T^2} statistic.}
#' \item{k}{Scaling factor for the squared Mahalanobis distance to obtain
#'   the \eqn{T^2} statistic.}
#' \item{T2}{Hotelling's \eqn{T^2} statistic (\eqn{F}-distributed).}
#' \item{F}{Observed \eqn{F} value.}
#' \item{F.crit}{Critical \eqn{F} value.}
#' \item{t.crit}{Critical \eqn{t} value.}
#' \item{p.F}{\eqn{p} value for Hotelling's \eqn{T^2} test statistic.}
#'
#' @references
#' Hotelling, H. The generalisation of Student's ratio. \emph{Ann Math Stat}.
#' 1931; \strong{2}(3): 360-378.
#'
#' Hotelling, H. (1947) \emph{Multivariate quality control illustrated by air
#' testing of sample bombsights}. In: Eisenhart, C., Hastay, M.W., and Wallis,
#' W.A., Eds., Techniques of Statistical Analysis, McGraw Hill, New York,
#' 111-184.
#'
#' @seealso \code{\link{get_T2_two}}, \code{\link{get_sim_lim}}.
#'
#' @example man/examples/examples_get_T2_one.R
#'
#' @importFrom stats cov
#' @importFrom stats pf
#' @importFrom stats qf
#' @importFrom stats qt
#'
#' @export

get_T2_one <- function(m, mu, signif, na_rm = FALSE) {
  if (!is.matrix(m)) {
    stop("The parameter m must be a matrix.")
  }
  if (!is.numeric(mu)) {
    stop("The parameter mu must be a numeric vector.")
  }
  if (ncol(m) != length(mu)) {
    stop("The number of columns in m must be the number of items in mu.")
  }
  if (!all(is.finite(mu))) {
    stop("Since mu contains NA/NaN/Inf values the assessment is not possible.")
  }
  if (signif <= 0 || signif > 1) {
    stop("Please specify signif as (0, 1]")
  }
  if (!is.logical(na_rm) || length(na_rm) > 1) {
    stop("The parameter na_rm must be a logical of length 1.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of data

  if (na_rm == TRUE) {
    m <- m[apply(m, 1, function(x) all(!is.na(x))), ]
  } else {
    if (any(is.na(m))) {
      message("Note that m contains NA/NaN values.\n",
              "  Please consider using the option na_rm = TRUE or\n",
              "  imputing missing values.")
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculation of various parameters

  # Number of profile time points (equal to sum(diag(solve(m_vc) %*% m_vc)))
  # or model parameters and number of observations of the reference group
  n_tp <- ncol(m)
  n_r <- nrow(m)

  # Covariance matrix of the reference group
  m_vc_r <- cov(m)

  # Average dissolution at a given time point or average model parameter
  # of the reference group
  mean_r <- apply(X = m, MARGIN = 2, FUN = mean, na.rm = na_rm)
  mean_diff <- mean_r - mu

  # Mahalanobis distance (dm)
  dm <- sqrt(t(mean_diff) %*% solve(m_vc_r) %*% mean_diff)

  # Degrees of freedom
  df1 <- n_tp
  df2 <- n_r - n_tp

  # Scaling factors for the calculation of the Hotelling's T2 statistic
  k <- n_r
  kk <- k * df2 / ((n_r - 1) * df1)

  # Hotelling's T2 statistic (general) and observed F value (ff_obs)
  tt2_value <- k * dm^2
  ff_obs <- kk * dm^2

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculation of critical F values

  # (1 - alpha) * 100th percentile of the F distribution with given degrees of
  # freedom
  ff_crit <- qf(p = 1 - signif, df1 = df1, df2 = df2)

  # (1 - alpha) * 100th percentile of the t distribution with given degrees of
  # freedom
  t_crit <- qt(p = 1 - signif / (2 * n_tp), df = n_r - 1)

  # Probability of seeing something as or even more extreme than ff_obs
  p_ff <- 1 - pf(ff_obs, df1 = df1, df2 = df2)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of confidence intervals

  l_ci <- list(
    Hotelling = data.frame(
      LCL = mean_r - sqrt(1 / kk * ff_crit * diag(m_vc_r)),
      UCL = mean_r + sqrt(1 / kk * ff_crit * diag(m_vc_r))
    ),
    Bonferroni = data.frame(
      LCL = mean_r - t_crit * sqrt(1 / k * diag(m_vc_r)),
      UCL = mean_r + t_crit * sqrt(1 / k * diag(m_vc_r))
    )
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compilation of results

  t_res <- c(dm, df1, df2, signif, kk, k, tt2_value, ff_obs,
             ff_crit, t_crit, p_ff)
  names(t_res) <- c("dm", "df1", "df2", "signif", "K", "k",
                    "T2", "F", "F.crit", "t.crit", "p.F")

  return(list(Parameters = t_res,
              cov = m_vc_r,
              means = list(mean.r = mean_r,
                           mean.t = mu,
                           mean.diff = mean_diff),
              CI = l_ci))
}

#' Hotelling's statistics (for two independent (small) samples)
#'
#' The function \code{get_hotellings()} estimates the parameters for Hotelling's
#' two-sample \eqn{T^2} statistic for small samples. \strong{Note that the
#' function \code{get_hotellings()} is deprecated}. Upon the introduction of
#' the new function \code{get_T2_one()} it was renamed to \code{get_T2_two()}.
#' Please use the new function \code{get_T2_two()} instead of the obsolete
#' function \code{get_hotellings()}.
#'
#' @inherit get_T2_two params details return references seealso
#'
#' @example man/examples/examples_get_hotellings.R
#'
#' @importFrom lifecycle deprecate_soft
#'
#' @export

get_hotellings <- function(m1, m2, signif) {
  deprecate_soft("0.1.4", "get_hotellings()", "get_T2_two()")

  res <- get_T2_two(m1 = m1, m2 = m2, signif = signif)

  return(res)
}

#' Hotelling's statistics (for two independent (small) samples)
#'
#' The function \code{get_T2_two()} estimates the parameters for Hotelling's
#' two-sample \eqn{T^2} statistic for small samples.
#'
#' @param m1 A matrix with the data of the reference group, e.g. a matrix
#'   representing dissolution profiles, i.e. with rows for the different dosage
#'   units and columns for the different time points, or a matrix for the
#'   different model parameters (columns) of different dosage units (rows).
#' @param m2 A matrix with the same dimensions as matrix \code{m1} with the
#'   data of the test group having the characteristics as the data of matrix
#'   \code{m1}.
#' @inheritParams get_T2_one
#'
#' @details The two-sample Hotelling's \eqn{T^2} test statistic is given by
#'
#' \deqn{T^2 = \frac{n_T n_R}{n_T + n_R} \left( \bm{x}_T - \bm{x}_R
#'   \right)^{\top} \bm{S}_{pooled}^{-1} \left( \bm{x}_T - \bm{x}_R \right) ,}{%
#'   (n_T n_R) / (n_T + n_R) * (x_T - x_R)^{\top} S_{pooled}^{-1} (x_T - x_R) ,}
#'
#' where \eqn{\bm{x}_T}{x_T} and \eqn{\bm{x}_R}{x_R} are the vectors of the
#' sample means of the test (\eqn{T}) and reference (\eqn{R}) group, e.g.
#' vectors of the average dissolution per time point or of the average model
#' parameters, \eqn{n_T} and \eqn{n_R} are the numbers of observations of the
#' reference and the test group, respectively (i.e. the number of rows in
#' matrices \code{m1} and \code{m2} handed over to the \code{get_T2_two()}
#' function), and \eqn{\bm{S}_{pooled}}{S_{pooled}} is the pooled
#' variance-covariance matrix which is calculated by
#'
#' \deqn{\bm{S}_{pooled} = \frac{(n_R - 1) \bm{S}_R + (n_T - 1) \bm{S}_T}{%
#'   n_R + n_T - 2} ,}{S_{pooled} = ((n_R - 1) S_R + (n_T - 1) S_T) /
#'   (n_R + n_T - 2) ,}
#'
#' where \eqn{\bm{S}_R}{S_R} and \eqn{\bm{S}_T}{S_T} are the estimated
#' variance-covariance matrices which are calculated from the matrices of the
#' two groups being compared, i.e. \code{m1} and \code{m2}. The matrix
#' \eqn{\bm{S}_{pooled}^{-1}}{S_{pooled}^{-1}} is the inverted
#' variance-covariance matrix. As the number of columns of matrices \code{m1}
#' and \code{m2} increases, and especially as the correlation between the
#' columns increases, the risk increases that the pooled variance-covariance
#' matrix \eqn{\bm{S}_{pooled}}{S_{pooled}} is ill-conditioned or even singular
#' and thus cannot be inverted. The term
#'
#' \deqn{D_M = \sqrt{ \left( \bm{x}_T - \bm{x}_R \right)^{\top}
#'   \bm{S}_{pooled}^{-1} \left( \bm{x}_T - \bm{x}_R \right) }}{%
#'   D_M = sqrt((x_T - x_R)^{\top} S_{pooled}^{-1} (x_T - x_R))}
#'
#' is the Mahalanobis distance which is used to measure the difference between
#' two multivariate means. For large samples, \eqn{T^2} is approximately
#' chi-square distributed with \eqn{p} degrees of freedom, where \eqn{p} is
#' the number of variables, i.e. the number of dissolution profile time points
#' or the number of model parameters. In terms of the Mahalanobis distance,
#' Hotelling's \eqn{T^2} statistic can be expressed has
#'
#' \deqn{\frac{n_T n_R}{n_T + n_R} \; D_M^2 = k \; D_M^2 .}
#'
#' To transform the Hotelling's \eqn{T^2} statistic into an \eqn{F}-statistic,
#' a conversion factor is necessary, i.e.
#'
#' \deqn{K = k \; \frac{n_T + n_R - p - 1}{\left( n_T + n_R - 2 \right) p} .}{%
#'   k (n_T + n_R - p - 1) / ((n_T + n_R - 2) p) .}
#'
#' With this transformation, the following test statistic can be applied:
#'
#' \deqn{K \; D_M^2 \leq F_{p, n_T + n_R - p - 1, \alpha} .}{%
#'   K D_M^2 \leq F_{p, n_T + n_R - p - 1, \alpha} .}
#'
#' Under the null hypothesis, \eqn{H_0: \bm{\mu}_T = \bm{\mu}_R}{%
#' H_0: \mu_T = \mu_R}, this \eqn{F}-statistic is \eqn{F}-distributed with
#' \eqn{p} and \eqn{n_T + n_R - p - 1} degrees of freedom. \eqn{H_0} is
#' rejected at significance level \eqn{\alpha} if the \eqn{F}-value exceeds
#' the critical value from the \eqn{F}-table evaluated at \eqn{\alpha}, i.e.
#' \eqn{F > F_{p, n_T + n_R - p - 1, \alpha}}. The null hypothesis is satisfied
#' if, and only if, the population means are identical for all variables. The
#' alternative is that at least one pair of these means is different. \cr
#'
#' The following assumptions concerning the data are made:
#' \itemize{
#' \item The data from population \eqn{i} is a sample from a population with
#'   mean vector \eqn{\mu_i}. In other words, it is assumed that there are no
#'   sub-populations.
#' \item The data from both populations have common variance-covariance matrix
#'   \eqn{\Sigma}.
#' \item The elements from both populations are independently sampled, i.e.
#'   the data values are independent.
#' \item Both populations are multivariate normally distributed.
#' }
#'
#' \strong{Confidence intervals}: \cr
#' Confidence intervals for the mean differences at each time point or
#' confidence intervals for the mean differences between the parameter
#' estimates of the reference and the test group are calculated by aid of the
#' formula
#'
#' \deqn{\left( \bm{x}_T - \bm{x}_R \right) \pm \sqrt{\frac{1}{K} \;
#'   F_{p, n_T + n_R - p - 1, \alpha} \; \bm{s}_{pooled}} ,}{%
#'   (x_T - x_R) \pm sqrt(1 / K F_{p, n_T + n_R - p - 1, \alpha} s_{pooled}) ,}
#'
#' where \eqn{\bm{s}_{pooled}}{s_{pooled}} is the vector of the diagonal
#' elements of the pooled variance-covariance matrix
#' \eqn{\bm{S}_{pooled}}{S_{pooled}}. With \eqn{(1 - \alpha)100\%} confidence,
#' this interval covers the respective linear combination of the differences
#' between the means of the two sample groups. If not the linear combination
#' of the variables is of interest but rather the individual variables, then
#' the Bonferroni corrected confidence intervals should be used instead which
#' are given by the expression
#'
#' \deqn{\left( \bm{x}_T - \bm{x}_R \right) \pm
#'   t_{n_T + n_R - 2, \frac{\alpha}{2 p}} \;
#'   \sqrt{\frac{1}{k} \; \bm{s}_{pooled}} .}{%
#'   (x_T - x_R) \pm t_{n_T + n_R - 2, \alpha / (2 p)} sqrt(1 / k s_{pooled}) .}
#'
#' @return A list with the following elements is returned:
#' \item{Parameters}{Parameters determined for the estimation of Hotelling's
#'   \eqn{T^2}.}
#' \item{S.pool}{Pooled variance-covariance matrix.}
#' \item{covs}{A list with the elements \code{S.b1} and \code{S.b2}, i.e. the
#'   variance-covariance matrices of the reference and the test group,
#'   respectively.}
#' \item{means}{A list with the elements \code{mean.b1}, \code{mean.b2} and
#'   \code{mean.diff}, i.e. the average dissolution profile values (for each
#'   time point) or the average model parameters of the reference and the test
#'   group and the corresponding differences, respectively.}
#' \item{CI}{A list with the elements \code{Hotelling} and \code{Bonferroni},
#'   i.e. data frames with columns \code{LCL} and \code{UCL} for the lower
#'   and upper \eqn{(1 - \alpha)100\%} confidence limits, respectively, and
#'   rows for each time point or model parameter.}
#'
#' The \code{Parameters} element contains the following information:
#' \item{dm}{Mahalanobis distance of the samples.}
#' \item{df1}{Degrees of freedom (number of variables or time points).}
#' \item{df2}{Degrees of freedom (number of rows - number of variables - 1).}
#' \item{alpha}{Provided significance level.}
#' \item{K}{Scaling factor for \eqn{F} to account for the distribution of the
#'   \eqn{T^2} statistic.}
#' \item{k}{Scaling factor for the squared Mahalanobis distance to obtain
#'   the \eqn{T^2} statistic.}
#' \item{T2}{Hotelling's \eqn{T^2} statistic (\eqn{F}-distributed).}
#' \item{F}{Observed \eqn{F} value.}
#' \item{F.crit}{Critical \eqn{F} value.}
#' \item{t.crit}{Critical \eqn{t} value.}
#' \item{p.F}{\eqn{p} value for Hotelling's \eqn{T^2} test statistic.}
#'
#' @references
#' Hotelling, H. The generalisation of Student's ratio. \emph{Ann Math Stat}.
#' 1931; \strong{2}(3): 360-378.
#'
#' Hotelling, H. (1947) \emph{Multivariate quality control illustrated by air
#' testing of sample bombsights}. In: Eisenhart, C., Hastay, M.W., and Wallis,
#' W.A., Eds., Techniques of Statistical Analysis, McGraw Hill, New York,
#' 111-184.
#'
#' @seealso \code{\link{get_T2_one}}, \code{\link{get_sim_lim}},
#' \code{\link{mimcr}}.
#'
#' @example man/examples/examples_get_T2_two.R
#'
#' @importFrom stats cov
#' @importFrom stats pf
#' @importFrom stats qf
#' @importFrom stats qt
#'
#' @export

get_T2_two <- function(m1, m2, signif, na_rm = FALSE) {
  if (!is.matrix(m1)) {
    stop("The sample m1 must be provided as matrix.")
  }
  if (!is.matrix(m2)) {
    stop("The sample m2 must be provided as matrix.")
  }
  if (!(ncol(m1) == ncol(m2))) {
    stop("The matrices m1 and m2 must have the same number of columns.")
  }
  if (signif <= 0 || signif > 1) {
    stop("Please specify signif as (0, 1]")
  }
  if (!is.logical(na_rm) || length(na_rm) > 1) {
    stop("The parameter na_rm must be a logical of length 1.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of data

  if (na_rm == TRUE) {
    m1 <- m1[apply(m1, 1, function(x) all(!is.na(x))), ]
    m2 <- m2[apply(m2, 1, function(x) all(!is.na(x))), ]
  } else {
    if (any(is.na(m1))) {
      message("Note that m1 contains NA/NaN values.\n",
              "  Please consider using the option na_rm = TRUE or\n",
              "  imputing missing values.")
    }
    if (any(is.na(m2))) {
      message("Note that m2 contains NA/NaN values.\n",
              "  Please consider using the option na_rm = TRUE or\n",
              "  imputing missing values.")
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculation of various parameters

  # Number of profile time points (equal to sum(diag(solve(m_vc) %*% m_vc)))
  # or model parameters and number of observations of the reference and test
  # group
  n_tp <- ncol(m1)
  n_b1 <- nrow(m1)
  n_b2 <- nrow(m2)

  # Covariance matrices of the reference and test group and their pooled
  # covariance matrix
  m_vc_b1 <- cov(m1)
  m_vc_b2 <- cov(m2)
  m_vc <- ((n_b1 - 1) * m_vc_b1 + (n_b2 - 1) * m_vc_b2) / (n_b1 + n_b2 - 2)

  # Average dissolution at a given time point or average model parameter
  # of the reference and test group and the corresponding difference vector
  mean_b1 <- apply(X = m1, MARGIN = 2, FUN = mean, na.rm = TRUE)
  mean_b2 <- apply(X = m2, MARGIN = 2, FUN = mean, na.rm = TRUE)
  mean_diff <- mean_b2 - mean_b1

  # Mahalanobis distance (dm)
  dm <- sqrt(t(mean_diff) %*% solve(m_vc) %*% mean_diff)

  # Degrees of freedom
  df1 <- n_tp
  df2 <- n_b1 + n_b2 - n_tp - 1

  # Scaling factors for the calculation of the Hotelling's T2 statistic
  k <- (n_b2 * n_b1) / (n_b2 + n_b1)
  kk <- k * df2 / ((n_b2 + n_b1 - 2) * df1)

  # Hotelling's T2 statistic (general) and observed F value (ff_obs)
  tt2_value <- k * dm^2
  ff_obs <- kk * dm^2

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculation of critical F values

  # (1 - alpha) * 100th percentile of the F distribution with given degrees of
  # freedom
  ff_crit <- qf(p = 1 - signif, df1 = df1, df2 = df2)

  # (1 - alpha) * 100th percentile of the t distribution with given degrees of
  # freedom
  t_crit <- qt(p = 1 - signif / (2 * n_tp), df = n_b1 + n_b2 - 2)

  # Probability of seeing something as or even more extreme than ff_obs
  p_ff <- 1 - pf(ff_obs, df1 = df1, df2 = df2)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of confidence intervals

  l_ci <- list(
    Hotelling = data.frame(
      LCL = mean_diff - sqrt(1 / kk * ff_crit * diag(m_vc)),
      UCL = mean_diff + sqrt(1 / kk * ff_crit * diag(m_vc))
    ),
    Bonferroni = data.frame(
      LCL = mean_diff - t_crit * sqrt(1 / k * diag(m_vc)),
      UCL = mean_diff + t_crit * sqrt(1 / k * diag(m_vc))
    )
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compilation of results

  t_res <- c(dm, df1, df2, signif, kk, k, tt2_value, ff_obs,
             ff_crit, t_crit, p_ff)
  names(t_res) <- c("dm", "df1", "df2", "signif", "K", "k",
                    "T2", "F", "F.crit", "t.crit", "p.F")

  return(list(Parameters = t_res,
              S.pool = m_vc,
              covs = list(S.b1 = m_vc_b1,
                          S.b2 = m_vc_b2),
              means = list(mean.b1 = mean_b1,
                           mean.b2 = mean_b2,
                           mean.diff = mean_diff),
              CI = l_ci))
}

#' Similarity limit
#'
#' The function \code{get_sim_lim()} estimates a similarity limit in terms of
#' the \dQuote{Multivariate Statistical Distance} (MSD).
#'
#' @param lhs A list of the estimates of Hotelling's two-sample \eqn{T^2}
#'   statistic for small samples as returned by the function
#'   \code{\link{get_T2_two}()}.
#' @inheritParams mimcr
#'
#' @details Details about the estimation of similarity limits in terms of
#' the \dQuote{Multivariate Statistical Distance} (MSD) are explained in
#' the corresponding section below.
#'
#' @inheritSection mimcr Similarity limits in terms of MSD
#'
#' @inheritSection mimcr T2 test for equivalence
#'
#' @return A vector containing the following information is returned:
#' \item{dm}{The Mahalanobis distance of the samples.}
#' \item{df1}{Degrees of freedom (number of variables or time points).}
#' \item{df2}{Degrees of freedom (number of rows - number of variables - 1).}
#' \item{alpha}{The provided significance level.}
#' \item{K}{Scaling factor for \eqn{F} to account for the distribution of the
#'   \eqn{T^2} statistic.}
#' \item{k}{Scaling factor for the squared Mahalanobis distance to obtain
#'   the \eqn{T^2} statistic.}
#' \item{T2}{Hotelling's \eqn{T^2} statistic (\eqn{F}-distributed).}
#' \item{F}{Observed \eqn{F} value.}
#' \item{ncp.Hoffelder}{Non-centrality parameter for calculation of the \eqn{F}
#'   statistic (\eqn{T^2} test procedure).}
#' \item{F.crit}{Critical \eqn{F} value (Tsong's procedure).}
#' \item{F.crit.Hoffelder}{Critical \eqn{F} value (\eqn{T^2} test procedure).}
#' \item{p.F}{The \eqn{p} value for the Hotelling's \eqn{T^2} test statistic.}
#' \item{p.F.Hoffelder}{The \eqn{p} value for the Hotelling's \eqn{T^2}
#'   statistic based on the non-central \eqn{F} distribution.}
#' \item{MTAD}{Specified \dQuote{maximum tolerable average difference} (MTAD)
#'   of the profiles of two formulations at each individual time point (in \%).}
#' \item{Sim.Limit}{Critical Mahalanobis distance or similarity limit
#'   (Tsong's procedure).}
#'
#' @references
#' Tsong, Y., Hammerstrom, T., Sathe, P.M., and Shah, V.P. Statistical
#' assessment of mean differences between two dissolution data sets.
#' \emph{Drug Inf J}. 1996; \strong{30}: 1105-1112.\cr
#' \doi{10.1177/009286159603000427}
#'
#' Wellek S. (2010) \emph{Testing statistical hypotheses of equivalence and
#' noninferiority} (2nd ed.). Chapman & Hall/CRC, Boca Raton.\cr
#' \doi{10.1201/EBK1439808184}
#'
#' Hoffelder, T. Highly variable dissolution profiles. Comparison of
#' \eqn{T^2}-test for equivalence and \eqn{f_2} based methods. \emph{Pharm Ind}.
#' 2016; \strong{78}(4): 587-592.\cr
#' \url{https://www.ecv.de/suse_item.php?suseId=Z|pi|8430}
#'
#' @seealso \code{\link{mimcr}}, \code{\link{get_T2_two}}.
#'
#' @example man/examples/examples_get_sim_lim.R
#'
#' @importFrom stats pf
#' @importFrom stats qf
#'
#' @export

get_sim_lim <- function(mtad, lhs) {
  if (!inherits(lhs, "list")) {
    stop("The parameter lhs must be a list returned by get_T2_two().")
  } else {
    if (sum(names(lhs) %in% c("Parameters", "S.pool", "covs", "means")) != 4) {
      stop("The parameter lhs must be a list returned by get_T2_two().")
    }
  }
  if (mtad <= 0 || mtad > 50) {
    stop("Please specify mtad as (0, 50]")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Similarity limit and critical F values

  hs <- lhs[[1]]

  # Global similarity limit d_crit determined according to Tsong 1996.
  # Note that d_glob is a vector of p * mtad specified as the global (or local)
  # similarity limit (in percent), i.e. the maximum tolerable average mean_diff
  # at all time points p.
  d_glob <- rep(mtad, times = hs["df1"])
  d_crit <- sqrt(t(d_glob) %*% solve(lhs[[2]]) %*% d_glob)

  # Non-centrality parameter that is based on  the equivalence region
  ncp_hoffelder <- hs["k"] * d_crit^2

  # alpha * 100th percentile of the F distribution with given degrees of freedom
  # and the
  ff_crit_hoffelder <-
    qf(p = hs["signif"], df1 = hs["df1"], df2 = hs["df2"], ncp = ncp_hoffelder)

  # Probability of seeing something as or even more extreme than ff_obs, given
  # the degrees of freedom and the non-centrality parameter defined by the
  # equivalence region
  p_ff_hoffelder <-
    pf(hs["F"], df1 = hs["df1"], df2 = hs["df2"], ncp = ncp_hoffelder)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compilation of results

  t_res <- c(hs["dm"], hs["df1"], hs["df2"], hs["signif"], hs["K"],
             hs["k"], hs["T2"], hs["F"], ncp_hoffelder, hs["F.crit"],
             ff_crit_hoffelder, hs["p.F"], p_ff_hoffelder, mtad, d_crit)
  names(t_res) <- c("dm", "df1", "df2", "alpha", "K", "k", "T2",
                    "F", "ncp.Hoffelder", "F.crit", "F.crit.Hoffelder",
                    "p.F", "p.F.Hoffelder", "MTAD", "Sim.Limit")
  return(t_res)
}

#' Dissimilarity factor f1 for dissolution data
#'
#' The function \code{f1()} calculates the dissimilarity factor \eqn{f_1}.
#'
#' @param use_ema A character string indicating whether the dissimilarity
#'   factor \eqn{f_1} should be calculated following the EMA guideline
#'   \dQuote{On the investigation of bioequivalence} (\code{"yes"}, the
#'   default) or not (\code{"no"}), i.e. the recommendations concerning the
#'   similarity factor \eqn{f_2}. A third option is \code{"ignore"}. If
#'   \code{use_ema} is \code{"yes"} or \code{"no"} the appropriate profile
#'   portion is determined on the basis of the values of the parameter
#'   \code{bounds}. If it is \code{"ignore"}, the complete profiles are used
#'   as specified by the parameter \code{tcol}.
#' @inheritParams bootstrap_f2
#'
#' @details Similarity of dissolution profiles is often assessed using the
#' similarity factor \eqn{f_2}, as recommended by the EMA guideline (European
#' Medicines Agency 2010) \dQuote{On the investigation of bioequivalence}. The
#' evaluation of the similarity factor is based on the following constraints:
#' \enumerate{
#'   \item A minimum of three time points (zero excluded).
#'   \item The time points should be the same for the two formulations.
#'   \item Twelve individual values for every time point for each formulation.
#'   \item Not more than one mean value of > 85\% dissolved for any of the
#'     formulations.
#'   \item The relative standard deviation or coefficient of variation of any
#'     product should be less than 20\% for the first time point and less than
#'     10\% from the second to the last time point.
#' }
#'
#' The \emph{dis}similarity factor, or difference factor, \eqn{f_1}, is the
#' counterpart of the similarity factor \eqn{f_2}. The difference factor
#' \eqn{f_1} is a measure of the relative error between two curves. Current FDA
#' guidelines suggest that two profiles can be considered similar if \eqn{f_1}
#' is less than \eqn{15} (\eqn{0 - 15}) and \eqn{f_2} is greater than \eqn{50}
#' (\eqn{50 - 100}), which is equivalent to an average difference of 10\% at
#' all sampling time points. The dissimilarity factor \eqn{f_1} is calculated
#' by aid of the equation
#'
#' \deqn{f_1 = 100 \times \frac{\sum_{t=1}^{n} \left( \left| \bar{R}(t) -
#'   \bar{T}(t) \right| \right)}{\sum_{t=1}^{n} (\bar{R}(t))} .}{%
#'   f_1 = 100 (sum(abs(R.bar(t) - T.bar(t))) / sum(R.bar(t))) .}
#'
#' In this equation
#' \describe{
#'   \item{\eqn{f_1}}{is the dissimilarity factor,}
#'   \item{\eqn{n}}{is the number of time points,}
#'   \item{\eqn{\bar{R}(t)}{R.bar(t)}}{is the mean percent reference drug
#'       dissolved at time \eqn{t} after initiation of the study, and}
#'   \item{\eqn{\bar{T}(t)}{T.bar(t)}}{is the mean percent test drug dissolved
#'       at time \eqn{t} after initiation of the study.}
#' }
#'
#' @return A list with the following elements is returned:
#' \item{f1}{A numeric value representing the similarity factor \eqn{f_1}.}
#' \item{Profile.TP}{A named numeric vector of the columns in \code{data}
#'   specified by \code{tcol} and depending on the selection of \code{use_ema}.
#'   Given that the column names contain extractable numeric information, e.g.,
#'   the testing time points of the dissolution profile, it contains the
#'   corresponding numeric values. Elements where no numeric information could
#'   be extracted are \code{NA}.}
#'
#' @references
#' United States Food and Drug Administration (FDA). Guidance for industry:
#' dissolution testing of immediate release solid oral dosage forms. 1997.\cr
#' \url{https://www.fda.gov/media/70936/download}
#'
#' United States Food and Drug Administration (FDA). Guidance for industry:
#' immediate release solid oral dosage form: scale-up and post-approval
#' changes, chemistry, manufacturing and controls, \emph{in vitro} dissolution
#' testing, and \emph{in vivo} bioequivalence documentation (SUPAC-IR). 1995.\cr
#' \url{https://www.fda.gov/media/70949/download}
#'
#' European Medicines Agency (EMA), Committee for Medicinal Products for
#' Human Use (CHMP). Guideline on the Investigation of Bioequivalence. 2010;
#' \href{https://www.ema.europa.eu/en/documents/scientific-guideline/guideline-investigation-bioequivalence-rev1_en.pdf}{
#' CPMP/EWP/QWP/1401/98 Rev. 1}.
#'
#' @seealso \code{\link{f2}}.
#'
#' @example man/examples/examples_f1.R
#'
#' @export

f1 <- function(data, tcol, grouping, use_ema = "yes", bounds = c(1, 85),
               nsf = c(1, 2)) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.numeric(tcol) || length(tcol) < 3) {
    stop("The parameter tcol must be an integer vector of at least length 3.")
  }
  if (!isTRUE(all.equal(tcol, as.integer(tcol)))) {
    stop("The parameter tcol must be an integer vector.")
  }
  if (min(tcol) < 1 || max(tcol) > ncol(data)) {
    stop("Some columns specified by tcol were not found in data frame.")
  }
  if (sum(grepl("\\d", colnames(data[, tcol]))) < length(tcol)) {
    stop("Some names of columns specified by tcol ",
         "do not contain numeric information.")
  }
  if (sum(vapply(data[, tcol], is.numeric, logical(1))) != length(tcol)) {
    stop("Some columns specified by tcol are not numeric.")
  }
  if (!is.character(grouping)) {
    stop("The parameter grouping must be string.")
  }
  if (!(grouping %in% colnames(data))) {
    stop("The grouping variable was not found in the provided data frame.")
  }
  if (!is.factor(data[, grouping])) {
    stop("The grouping variable's column in data must be a factor.")
  }
  if (!(use_ema %in% c("yes", "no", "ignore"))) {
    stop("Please specify use_ema either as \"yes\" or \"no\" or \"ignore\".")
  }
  if (!is.numeric(bounds) || length(bounds) != 2) {
    stop("The parameter bounds must be a numeric vector of length 2.")
  }
  if (bounds[1] > bounds[2]) {
    stop("Please specify bounds in the form c(lower limit, upper limit).")
  }
  if (bounds[1] < 0 || bounds[2] > 100) {
    stop("Please specify bounds in the range [0, 100].")
  }
  if (!is.numeric(nsf) && any(!is.na(nsf))) {
    stop("The parameter nsf must be a positive integer of length bounds.")
  }
  if (any(nsf < 0)) {
    stop("The parameter nsf must be a positive integer of length bounds.")
  }
  if (length(nsf) != length(bounds)) {
    stop("The parameter nsf must be a positive integer of length bounds.")
  }
  if (!isTRUE(all.equal(nsf, as.integer(nsf)))) {
    stop("The parameter nsf must be a positive integer of length bounds.")
  }

  # <-><-><-><->

  data <- droplevels(data)

  if (nlevels(data[, grouping]) != 2) {
    stop("The number of levels in column ", grouping, " differs from 2.")
  }

  # <-><-><-><->

  # Generation of logical vector representing the reference and test group
  b1 <- make_grouping(data = data, grouping = grouping)

  # Check if the two groups have the same number of observations. If  not so,
  # and if the parameter use_ema is either "no" or "ignore", adjust the data
  # frames in such a way that both groups to be compared will have the same
  # number of observations and that the number of observations per group
  # corresponds to the largest common value (lcv) between number of observations
  # per group.
  # Note: an alternative would be to use the least common multiple.
  # Note that together with the data adjustment the b1 vector must be reset.

  if (use_ema == "yes" && sum(b1) != sum(!b1)) {
    stop("The two groups to be compared must have the same number of ",
         "observations.")
  }
  if (use_ema %in% c("no", "ignore") && sum(b1) != sum(!b1)) {
    warning("The two groups to be compared do not have the same number of\n",
            "  observations. Thus, the number of rows is adjusted according\n",
            "  to the largest common value between the number of\n",
            "  observations per group.")

    data <- balance_observations(data = data, groups = b1,
                                 n_obs = max(sum(b1), sum(!b1)))
    b1 <- make_grouping(data = data, grouping = grouping)
  }

  # Note: the following requirements prescribed by the EMA:
  # A minimum of three time points (zero excluded).
  # The time points should be the same for the two formulations.
  # Twelve individual values for every time point for each formulation.
  # Not more than one mean value > 85% dissolved for any of the formulations.
  # The relative standard deviation or coefficient of variation of any product
  # should be
  #   less than 20% for the first time point and
  #   less than 10% from the second to the last time point.

  ok <- get_profile_portion(data = data, tcol = tcol, groups = b1,
                            use_ema = use_ema, bounds = bounds, nsf = nsf)

  if (use_ema == "yes" && sum(ok) < 3) {
    stop("According to EMA the profiles must comprise a minimum of 3 time\n",
         "  points. The actual profiles comprise ", sum(ok), " points only.")
  }
  if (sum(ok) < 3) {
    warning("The profiles should comprise a minimum of 3 time points.\n",
            "  The actual profiles comprise ", sum(ok), " points only.")
  }

  # <-><-><-><->
  # Extraction of information on the time points
  time_points <- get_time_points(svec = colnames(data)[tcol])
  time_points <- time_points[ok]

  # <-><-><-><->
  # Calculation of f1

  t_f1 <- get_f1(data = data, ins = seq_along(b1), tcol = tcol[ok],
                 grouping = grouping)

  # <-><-><-><->
  # Compilation of results

  return(list(f1 = t_f1,
              Profile.TP = time_points))
}

#' Get the f1 dissimilarity factor
#'
#' The function \code{get_f1()} calculates the dissimilarity factor \eqn{f_1}
#' for the assessment of dissolution profiles.
#'
#' @param ins A vector of indices that specifies the rows in \code{data}
#'   which should be used for the assessment.
#' @inheritParams bootstrap_f2
#'
#' @inherit f1 details references
#'
#' @return The function returns the \eqn{f_1} value.
#'
#' @seealso \code{\link{get_f2}}.
#'
#' @keywords internal
#' @noRd

get_f1 <- function(data, ins, tcol, grouping) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.numeric(ins) || length(ins) < 3 || length(ins) > nrow(data)) {
    stop("The parameter ins must be an integer vector not longer than ",
         "nrow(data).")
  }
  if (!isTRUE(all.equal(ins, as.integer(ins)))) {
    stop("The parameter ins must be an integer vector not longer than ",
         "nrow(data).")
  }
  if (!is.numeric(tcol) || length(tcol) < 3) {
    stop("The parameter tcol must be an integer vector of at least length 3.")
  }
  if (!isTRUE(all.equal(tcol, as.integer(tcol)))) {
    stop("The parameter tcol must be an integer vector.")
  }
  if (min(tcol) < 1 || max(tcol) > ncol(data)) {
    stop("Some columns specified by tcol were not found in data frame.")
  }
  if (sum(grepl("\\d", colnames(data[, tcol]))) < length(tcol)) {
    stop("Some names of columns specified by tcol ",
         "do not contain numeric information.")
  }
  if (sum(vapply(data[, tcol], is.numeric, logical(1))) != length(tcol)) {
    stop("Some columns specified by tcol are not numeric.")
  }
  if (!is.character(grouping)) {
    stop("The parameter grouping must be string.")
  }
  if (!(grouping %in% colnames(data))) {
    stop("The grouping variable was not found in the provided data frame.")
  }
  if (!is.factor(data[, grouping])) {
    stop("The grouping variable's column in data must be a factor.")
  }

  # <-><-><-><->

  data <- droplevels(data)

  if (nlevels(data[, grouping]) != 2) {
    stop("The number of levels in column ", grouping, " differs from 2.")
  }

  if (any(is.na(data[, tcol]))) {
    message("Note that data contains NA/NaN values.\n",
            "  Please consider imputing missing values.")
  }

  # <-><-><-><->

  b1 <- make_grouping(data = data[ins, ], grouping = grouping)

  rr_i <- apply(data[ins, ][b1, tcol], MARGIN = 2, FUN = mean)
  tt_i <- apply(data[ins, ][!b1, tcol], MARGIN = 2, FUN = mean)

  ddelta_hat <- sum(abs(rr_i - tt_i))
  f1 <- 100 * ddelta_hat / sum(rr_i)

  return(f1)
}

#' Similarity factor f2 for dissolution data
#'
#' The function \code{f2()} calculates the similarity factor \eqn{f_2}.
#'
#' @inheritParams f1
#'
#' @details Similarity of dissolution profiles is assessed using the similarity
#' factor \eqn{f_2} according to the EMA guideline (European Medicines Agency
#' 2010) \dQuote{On the investigation of bioequivalence}. The evaluation of the
#' similarity factor is based on the following constraints:
#' \enumerate{
#'   \item A minimum of three time points (zero excluded).
#'   \item The time points should be the same for the two formulations.
#'   \item Twelve individual values for every time point for each formulation.
#'   \item Not more than one mean value of > 85\% dissolved for any of the
#'     formulations.
#'   \item The relative standard deviation or coefficient of variation of any
#'     product should be less than 20\% for the first time point and less than
#'     10\% from the second to the last time point.
#' }
#'
#' The similarity factor \eqn{f_2} is calculated by aid of the equation
#'
#' \deqn{f_2 = 50 \times \log \left(\frac{100}{\sqrt{1 + \frac{\sum_{t=1}^{n}
#'   \left(\bar{R}(t) - \bar{T}(t) \right)^2}{n}}} \right) .}{%
#'   f_2 = 50 log(100 / (sqrt(1 + (sum((R.bar(t) - T.bar(t))^2) / n)))) .}
#'
#' In this equation
#' \describe{
#'   \item{\eqn{f_2}}{is the similarity factor,}
#'   \item{\eqn{n}}{is the number of time points,}
#'   \item{\eqn{\bar{R}(t)}{R.bar(t)}}{is the mean percent reference drug
#'       dissolved at time \eqn{t} after initiation of the study, and}
#'   \item{\eqn{\bar{T}(t)}{T.bar(t)}}{is the mean percent test drug dissolved
#'       at time \eqn{t} after initiation of the study.}
#' }
#'
#' Dissolution profiles are regarded as similar if the \eqn{f_2} value is
#' between \eqn{50} and \eqn{100}. \cr
#'
#' @return A list with the following elements is returned:
#' \item{f2}{A numeric value representing the similarity factor \eqn{f_2}.}
#' \item{Profile.TP}{A named numeric vector of the columns in \code{data}
#'   specified by \code{tcol} and depending on the selection of \code{use_ema}.
#'   Given that the column names contain extractable numeric information, e.g.,
#'   the testing time points of the dissolution profile, it contains the
#'   corresponding numeric values. Elements where no numeric information could
#'   be extracted are \code{NA}.}
#'
#' @references
#' United States Food and Drug Administration (FDA). Guidance for industry:
#' dissolution testing of immediate release solid oral dosage forms. 1997.\cr
#' \url{https://www.fda.gov/media/70936/download}
#'
#' United States Food and Drug Administration (FDA). Guidance for industry:
#' immediate release solid oral dosage form: scale-up and post-approval
#' changes, chemistry, manufacturing and controls, \emph{in vitro} dissolution
#' testing, and \emph{in vivo} bioequivalence documentation (SUPAC-IR). 1995.\cr
#' \url{https://www.fda.gov/media/70949/download}
#'
#' European Medicines Agency (EMA), Committee for Medicinal Products for
#' Human Use (CHMP). Guideline on the Investigation of Bioequivalence. 2010;
#' \href{https://www.ema.europa.eu/en/documents/scientific-guideline/guideline-investigation-bioequivalence-rev1_en.pdf}{
#' CPMP/EWP/QWP/1401/98 Rev. 1}.
#'
#' @seealso \code{\link{f1}}.
#'
#' @example man/examples/examples_f2.R
#'
#' @export

f2 <- function(data, tcol, grouping, use_ema = "yes", bounds = c(1, 85),
               nsf = c(1, 2)) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.numeric(tcol) || length(tcol) < 3) {
    stop("The parameter tcol must be an integer vector of at least length 3.")
  }
  if (!isTRUE(all.equal(tcol, as.integer(tcol)))) {
    stop("The parameter tcol must be an integer vector.")
  }
  if (min(tcol) < 1 || max(tcol) > ncol(data)) {
    stop("Some columns specified by tcol were not found in data frame.")
  }
  if (sum(grepl("\\d", colnames(data[, tcol]))) < length(tcol)) {
    stop("Some names of columns specified by tcol ",
         "do not contain numeric information.")
  }
  if (sum(vapply(data[, tcol], is.numeric, logical(1))) != length(tcol)) {
    stop("Some columns specified by tcol are not numeric.")
  }
  if (!is.character(grouping)) {
    stop("The parameter grouping must be string.")
  }
  if (!(grouping %in% colnames(data))) {
    stop("The grouping variable was not found in the provided data frame.")
  }
  if (!is.factor(data[, grouping])) {
    stop("The grouping variable's column in data must be a factor.")
  }
  if (!(use_ema %in% c("yes", "no", "ignore"))) {
    stop("Please specify use_ema either as \"yes\" or \"no\" or \"ignore\".")
  }
  if (!is.numeric(bounds) || length(bounds) != 2) {
    stop("The parameter bounds must be a numeric vector of length 2.")
  }
  if (bounds[1] > bounds[2]) {
    stop("Please specify bounds in the form c(lower limit, upper limit).")
  }
  if (bounds[1] < 0 || bounds[2] > 100) {
    stop("Please specify bounds in the range [0, 100].")
  }

  # <-><-><-><->

  data <- droplevels(data)

  if (nlevels(data[, grouping]) != 2) {
    stop("The number of levels in column ", grouping, " differs from 2.")
  }

  # <-><-><-><->

  # Generation of logical vector representing the reference and test group
  b1 <- make_grouping(data = data, grouping = grouping)

  # Check if the two groups have the same number of observations. If  not so,
  # and if the parameter use_ema is either "no" or "ignore", adjust the data
  # frames in such a way that both groups to be compared will have the same
  # number of observations and that the number of observations per group
  # corresponds to the largest common value (lcv) between number of observations
  # per group.
  # Note: an alternative would be to use the least common multiple.
  # Note that together with the data adjustment the b1 vector must be reset.

  if (use_ema == "yes" && sum(b1) != sum(!b1)) {
    stop("The two groups to be compared must have the same number of ",
         "observations.")
  }
  if (use_ema %in% c("no", "ignore") && sum(b1) != sum(!b1)) {
    warning("The two groups to be compared do not have the same number of\n",
            "  observations. Thus, the number of rows is adjusted according\n",
            "  to the largest common value between the number of\n",
            "  observations per group.")

    data <- balance_observations(data = data, groups = b1,
                                 n_obs = max(sum(b1), sum(!b1)))
    b1 <- make_grouping(data = data, grouping = grouping)
  }

  # Note: the following requirements prescribed by the EMA:
  # A minimum of three time points (zero excluded).
  # The time points should be the same for the two formulations.
  # Twelve individual values for every time point for each formulation.
  # Not more than one mean value > 85% dissolved for any of the formulations.
  # The relative standard deviation or coefficient of variation of any product
  # should be
  #   less than 20% for the first time point and
  #   less than 10% from the second to the last time point.

  ok <- get_profile_portion(data = data, tcol = tcol, groups = b1,
                            use_ema = use_ema, bounds = bounds, nsf = nsf)

  if (use_ema == "yes" && sum(ok) < 3) {
    stop("According to EMA the profiles must comprise a minimum of 3 time\n",
         "  points. The actual profiles comprise ", sum(ok), " points only.")
  }
  if (sum(ok) < 3) {
    warning("The profiles should comprise a minimum of 3 time points.\n",
            "  The actual profiles comprise ", sum(ok), " points only.")
  }

  # <-><-><-><->
  # Extraction of information on the time points
  time_points <- get_time_points(svec = colnames(data)[tcol])
  time_points <- time_points[ok]

  # <-><-><-><->
  # Calculation of f2

  t_f2 <- get_f2(data = data, ins = seq_along(b1), tcol = tcol[ok],
                 grouping = grouping)

  # <-><-><-><->
  # Compilation of results

  return(list(f2 = t_f2,
              Profile.TP = time_points))
}

#' Get the f2 similarity factor
#'
#' The function \code{get_f2()} calculates the similarity factor \eqn{f_2} for
#' the assessment of dissolution profiles.
#'
#' @inheritParams get_f1
#'
#' @inherit f2 details references
#'
#' @return The function returns the \eqn{f_2} value.
#'
#' @seealso \code{\link{get_f1}}.
#'
#' @keywords internal
#' @noRd

get_f2 <- function(data, ins, tcol, grouping) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.numeric(ins) || length(ins) < 3 || length(ins) > nrow(data)) {
    stop("The parameter ins must be an integer vector not longer than ",
         "nrow(data).")
  }
  if (!isTRUE(all.equal(ins, as.integer(ins)))) {
    stop("The parameter ins must be an integer vector not longer than ",
         "nrow(data).")
  }
  if (!is.numeric(tcol) || length(tcol) < 3) {
    stop("The parameter tcol must be an integer vector of at least length 3.")
  }
  if (!isTRUE(all.equal(tcol, as.integer(tcol)))) {
    stop("The parameter tcol must be an integer vector.")
  }
  if (min(tcol) < 1 || max(tcol) > ncol(data)) {
    stop("Some columns specified by tcol were not found in data frame.")
  }
  if (sum(grepl("\\d", colnames(data[, tcol]))) < length(tcol)) {
    stop("Some names of columns specified by tcol ",
         "do not contain numeric information.")
  }
  if (sum(vapply(data[, tcol], is.numeric, logical(1))) != length(tcol)) {
    stop("Some columns specified by tcol are not numeric.")
  }
  if (!is.character(grouping)) {
    stop("The parameter grouping must be string.")
  }
  if (!(grouping %in% colnames(data))) {
    stop("The grouping variable was not found in the provided data frame.")
  }
  if (!is.factor(data[, grouping])) {
    stop("The grouping variable's column in data must be a factor.")
  }

  # <-><-><-><->

  data <- droplevels(data)

  if (nlevels(data[, grouping]) != 2) {
    stop("The number of levels in column ", grouping, " differs from 2.")
  }

  if (any(is.na(data[, tcol]))) {
    message("Note that data contains NA/NaN values.\n",
            "  Please consider imputing missing values.")
  }

  # <-><-><-><->

  n <- length(tcol)
  b1 <- make_grouping(data = data[ins, ], grouping = grouping)

  rr_i <- apply(data[ins, ][b1, tcol], MARGIN = 2, FUN = mean)
  tt_i <- apply(data[ins, ][!b1, tcol], MARGIN = 2, FUN = mean)

  ddelta_hat <- 1 + sum((rr_i - tt_i)^2) / n
  f2 <- 50 * log10(100 / sqrt(ddelta_hat))

  return(f2)
}
