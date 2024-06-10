#' Bootstrap f2
#'
#' The function \code{bootstrap_f2()} generates \code{rr} bootstrap replicates
#' of the similarity factor \eqn{f_2} based on resampling of complete profiles
#' (nonparametric bootstrap) or on resampling per time point the values
#' between profiles (parametric bootstrap). Estimates of \dQuote{normal},
#' \dQuote{basic}, \dQuote{student}, \dQuote{percent} and of
#' \dQuote{bias-corrected, accelerated} (BCa) percentile intervals are returned.
#'
#' @param tcol A vector of indices that specifies the columns in \code{data}
#'   that contain the \% release values. The length of \code{tcol} must be
#'   three or longer.
#' @param rand_mode A character string that indicates whether complete profiles
#'   shall be randomised (\code{"complete"}, the default) or individual data
#'   points (\code{"individual"}).
#' @param rr An integer that specifies the number of bootstrap replicates. The
#'   default is \code{999}.
#' @param each An integer that specifies the number of dissolution profiles to
#'   be selected per group per randomisation round. The default is \code{12}.
#' @param new_seed An integer for setting the seed for random number generation.
#'   The default is \code{100}.
#' @param confid A numeric value between 0 and 1 that specifies the confidence
#'   limit for the calculation of the bootstrap confidence intervals. The
#'   default is \code{0.9}.
#' @param use_ema A character string that indicates whether the similarity
#'   factor \eqn{f_2} should be calculated according to the EMA guideline
#'   \dQuote{On the investigation of bioequivalence} (\code{"yes"}) or not
#'   (\code{"no"}, the default). The default is \code{"no"} because the
#'   bootstrap \eqn{f_2} method is one of the possible solutions if the
#'   condition concerning the variability between the profiles does not allow
#'   the evaluation of \eqn{f_2} according to the EMA guideline. A third option
#'   is \code{"ignore"}. If \code{use_ema} is \code{"yes"}, the \code{bounds}
#'   are \code{c(0, 85)} per definition. If \code{use_ema} is \code{"no"}, the
#'   appropriate profile portion is determined on the basis of the values of
#'   the parameter \code{bounds}. If \code{use_ema} is \code{"ignore"}, the
#'   complete profiles are used as specified by the parameter \code{tcol}.
#' @param bounds A numeric vector of the form \code{c(lower, upper)} that
#'   specifies the \dQuote{lower} and \dQuote{upper} limits, respectively, for
#'   the \% drug release given that \code{use_ema} is \code{"no"}. The default
#'   is \code{c(1, 85)}. Mean \% release values of any of the two groups being
#'   compared that are smaller than or equal to the lower bound are ignored and
#'   only the first mean \% release value that is greater than or equal to the
#'   upper bound is included while all the subsequent values are ignored. If
#'   \code{use_ema} is \code{"yes"} the \code{bounds} are \code{c(0, 85)} per
#'   definition. If \code{use_ema} is \code{"ignore"} the \code{bounds} are
#'   disregarded.
#' @param nsf A vector of positive integers that specify the \dQuote{number
#'   of significant figures} (nsf) of the corresponding values of the
#'   \code{bounds} parameter. It must thus have the same length as the
#'   \code{bounds} parameter. Before the \% release values are compared with
#'   the limits that are specified by the \code{bounds} parameter, they are
#'   rounded to the corresponding number of significant figures as specified
#'   by the \code{nsf} parameter.
#' @param ... Named parameters of the functions \code{stat.fun()},
#'   \code{ran.fun()} and \code{boot()}.
#' @inheritParams mimcr
#'
#' @details Information on \eqn{f_2} can be found in at least three FDA
#' guidances and in the guideline of the European Medicines Agency (EMA)
#' \dQuote{On the investigation of bioequivalence} (EMA 2010). For the
#' assessment of the similarity of dissolution profiles using the similarity
#' factor \eqn{f_2} according to the EMA guideline the following constraints
#' do apply:
#' \enumerate{
#' \item A minimum of three time points (without zero) are necessary.
#' \item The time points should be the same for the two formulations.
#' \item For every time point and for each formulation at least 12 data points
#'   are required.
#' \item A maximum of one mean value per formulation may be > 85\% dissolved.
#' \item The coefficient of variation (\%CV) should be < 20\% for the first
#'   time point and < 10\% from the second to the last time point for any
#'   formulation.
#' }
#'
#' Dissolution profiles are regarded as similar if the \eqn{f_2} value is
#' between 50 and 100. \cr
#'
#' One often encountered problem is that the \%CV constraint cannot be
#' fulfilled. One possibility in this situation is the use of the bootstrap
#' \eqn{f_2} method (Shah 1998) by which the distribution of \eqn{f_2} is
#' simulated to obtain an unbiased estimate of the expected value of \eqn{f_2}
#' and the variability of the underlying distribution. For the \eqn{f_2}
#' calculation only those parts of the profiles are taken into account where
#' the means (per formulation) are \eqn{> d}\% dissolved (e.g., \eqn{d = 1})
#' and a maximum of one mean value per formulation is \eqn{> 85}\% dissolved.
#' In the literature it is suggested to make use of the lower 90\% bias
#' corrected and accelerated (BCa) confidence interval (CI) limit to come to
#' a decision in terms of similarity (Stevens (2015)).
#'
#' @return An object of class \sQuote{\code{bootstrap_f2}} is returned,
#' containing the following list elements:
#' \item{Boot}{An object of class \sQuote{\code{boot}} with the corresponding
#'   components.}
#' \item{Profile.TP}{A named numeric vector of the columns in \code{data}
#'   specified by \code{tcol} and depending on the selection of \code{use_ema}.
#'   Given that the column names contain extractable numeric information, e.g.,
#'   the testing time points of the dissolution profile, it contains the
#'   corresponding numeric values. Elements where no numeric information could
#'   be extracted are \code{NA}.}
#' \item{L}{A vector of the Jackknife leave-one-out-values.}
#' \item{CI}{An object of class \sQuote{\code{bootci}} which contains the
#'   intervals.}
#' \item{BCa_CI}{The lower and upper limits of the BCa interval calculated
#'   by the \code{boot.ci()} function from the \sQuote{\code{boot}} package.}
#' \item{Shah_BCa_CI}{The lower and upper limits of the BCa interval calculated
#'   according to Shah (Shah 1998).}
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
#' Stevens, R. E., Gray, V., Dorantes, A., Gold, L., and Pham, L. Scientific
#' and regulatory standards for assessing product performance using the
#' similarity factor, \eqn{f_2}. \emph{AAPS Journal}. 2015; \strong{17}(2):
#' 301-306.\cr
#' \doi{10.1208/s12248-015-9723-y}
#'
#' Shah, V. P., Tsong, Y., Sathe, P., and Liu, J. P. \emph{In vitro} dissolution
#' profile comparison - statistics and analysis of the similarity factor,
#' \eqn{f_2}. \emph{Pharm Res}. 1998; \strong{15}(6): 889-896.\cr
#' \doi{10.1023/A:1011976615750}
#'
#' @seealso \code{\link[boot]{boot}}, \code{\link[boot]{boot.ci}},
#'   \code{\link{mimcr}}, \code{\link{mztia}}.
#'
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom stats quantile
#' @importFrom boot boot
#' @importFrom boot boot.ci
#'
#' @export

bootstrap_f2 <- function(data, tcol, grouping, rand_mode = "complete",
                   rr = 999, each = 12, new_seed = 100, confid = 0.9,
                   use_ema = "no", bounds = c(1, 85), nsf = c(1, 2), ...) {
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
  if (sum(vapply(data[, tcol], is.numeric, logical(1))) != length(tcol)) {
    stop("Some columns specified by tcol are not numeric.")
  }
  if (any(is.na(data[, tcol]))) {
    stop("Note that data contains NA/NaN values.\n",
            "  Please consider imputing missing values.")
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
  if (!(rand_mode %in% c("complete", "individual"))) {
    stop("Please specify rand_mode either as \"complete\" or \"individual\".")
  }
  if (!is.numeric(rr) || length(rr) > 1) {
    stop("The parameter rr must be an integer of length 1.")
  }
  if (!isTRUE(all.equal(rr, as.integer(rr)))) {
    stop("The parameter rr must be an integer of length 1.")
  }
  if (!is.numeric(each) || length(each) > 1) {
    stop("The parameter each must be an integer of length 1.")
  }
  if (!isTRUE(all.equal(each, as.integer(each)))) {
    stop("The parameter each must be an integer of length 1.")
  }
  if (!is.numeric(new_seed) || length(new_seed) > 1) {
    stop("The parameter new_seed must be an integer of length 1.")
  }
  if (!isTRUE(all.equal(new_seed, as.integer(new_seed)))) {
    stop("The parameter new_seed must be an integer of length 1.")
  }
  if (confid <= 0 || confid > 1) {
    stop("Please specify confid as (0, 1]")
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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Data preparation

  data <- droplevels(data)

  if (nlevels(data[, grouping]) != 2) {
    stop("The number of levels in column ", grouping, " differs from 2.")
  }

  # <-><-><-><->
  # Extraction of information on the time points
  time_points <- get_time_points(svec = colnames(data)[tcol])

  # <-><-><-><->
  # Generation of logical vector representing the reference and test group
  b1 <- make_grouping(data = data, grouping = grouping)

  # Check if the two groups have the same number of observations and if the
  # parameter "each" is a multiple of the number of observations per group.
  # If the parameter 'each' is not a mulitple of the number of dissolution
  # profiles of the two groups or if the two groups do not have the same number
  # of observations, adjust the data frame. Do the adjustment in such a way that
  # both groups to be compared will have the same number of observations and
  # that the number of observations per group corresponds to the largest common
  # value (lcv) between 'each' and the number of observations per group.
  # Note: an alternative would be to use the least common multiple between the
  #       three numbers.
  # Note that together with the data adjustment the b1 vector must be reset.

  if (2 * each > nrow(data) || sum(b1) != sum(!b1)) {
    warning("The two groups to be compared do not have the same number of\n",
            "  observations. Thus, the number of rows is adjusted according\n",
            "  to the largest common value between the parameter each and\n",
            "  the number of observations per group.")

    data <- balance_observations(data = data, groups = b1, n_obs = each)
    b1 <- make_grouping(data = data, grouping = grouping)
  }

  # <-><-><-><->
  # Determination of dissolution profile ranges to be compared
  ok <- get_profile_portion(data = data, tcol = tcol, groups = b1,
                            use_ema = use_ema, bounds = bounds, nsf = nsf)
  time_points <- time_points[ok]

  if (use_ema == "yes" && sum(ok) < 3) {
    stop("According to EMA the profiles must comprise a minimum of 3 time\n",
         "  points. The actual profiles comprise ", sum(ok), " points only.")
  }
  if (sum(ok) < 3) {
    warning("The profiles should comprise a minimum of 3 time points.\n",
            "  The actual profiles comprise ", sum(ok), " points only.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Bootstrap assessment

  set.seed(new_seed)
  switch(rand_mode, "complete" = {
    t_boot <- boot(data = data, statistic = get_f2, R = rr,
                   strata = data[, grouping], grouping = grouping,
                   tcol = tcol[ok], ...)
  }, "individual" = {
    mle <- vector(mode = "list", length = 2)
    mle[[1]] <- nrow(data) / 2
    mle[[2]] <- tcol[ok]
    t_boot <- boot(data = data, statistic = get_f2, R = rr,
                   sim = "parametric", ran.gen = rand_indiv_points,
                   mle = mle, grouping = grouping, tcol = tcol[ok],
                   ins = seq_along(b1), ...)
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Bootstrap confidence intervals

  jack <- get_jackknife_values(grouping = grouping, stat_fun = get_f2,
                               data = data, tcol = tcol[ok])

  cis <- suppressWarnings(boot.ci(t_boot, conf = confid, type = "all",
                                        L = jack$loo.values))

  # Shah's BCa corrected confid * 100% CI (see Shah et al., Pharm Res, 1998)
  a <- (sum((jack$theta.jack - jack$loo.values)^3)) /
    (sum(b1) / 2 *
       sum((jack$theta.jack - jack$loo.values)^2)^(3 / 2))
  z0 <- qnorm(sum(t_boot$t < t_boot$t0) / rr)

  bca_ll <- pnorm(z0 + (z0 + qnorm((1 - confid) / 2)) /
                    (1 - a * (z0 + qnorm((1 - confid) / 2))))
  bca_ul <- pnorm(z0 + (z0 + qnorm(1 - (1 - confid) / 2)) /
                    (1 - a * (z0 + qnorm(1 - (1 - confid) / 2))))
  shah_bca_ci <- as.numeric(c(quantile(t_boot$t, bca_ll),
                             quantile(t_boot$t, bca_ul)))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compilation of results

  structure(list(Boot = t_boot,
                 Profile.TP = time_points,
                 L = jack$loo.values,
                 CI = cis,
                 BCa_CI = as.numeric(cis$bca[4:5]),
                 Shah_BCa_CI = shah_bca_ci),
            class = "bootstrap_f2")
}

#' Jackknife values
#'
#' The function \code{get_jackknife_values()} is required for generation of
#' vector \eqn{L} for the BCa confidence interval estimation by the
#' \code{boot.ci()} function from the \sQuote{\code{boot}} package.
#'
#' @param stat_fun The name of the function calculating the statistic of
#'   interest. The statistic function must return a single number.
#' @param data A data frame with the dissolution profile data in wide format
#'   and a column for the distinction of the groups to be compared.
#' @param ... Further arguments that may be passed down to the statistics
#'   function.
#' @inheritParams bootstrap_f2
#'
#' @details For the calculation of BCa bootstrap confidence intervals empirical
#' influence values of the statistic of interest for the observed data are
#' required. If \eqn{L} is not provided to the \code{boot.ci()} function from
#' the \sQuote{\code{boot}} package, the values are calculated, if needed,
#' using the \code{empinf()} function (also from the \sQuote{\code{boot}}
#' package). Since the results returned by \code{empinf()} are not appropriate
#' in this case, the \eqn{L} values are determined using
#' \code{get_jackknife_values()}. The jackknife calculations are done by
#' calculating the leave-one-out estimates.
#'
#' @return A list of the jackknife assessment with the following elements is
#' returned:
#' \item{theta.hat}{The original value of the statistic.}
#' \item{theta.jack}{The jackknife value of the statistic.}
#' \item{jack.se}{The jackknife standard error of the statistic.}
#' \item{jack.bias}{The bias of the statistic.}
#' \item{loo.values}{The leave-one-out values.}
#' \item{pseudo.values}{The pseudo values.}
#'
#' @references
#' Chen, H. Bootstrap and Jackknife Calculations in R. 2004.
#'
#' @seealso \code{\link{bootstrap_f2}}, \code{\link[boot]{boot}},
#'   \code{\link[boot]{boot.ci}}.
#'
#' @keywords internal
#' @noRd

get_jackknife_values <- function(grouping, stat_fun, data, ...) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!(grouping %in% colnames(data))) {
    stop("The grouping variable was not found in the provided data frame.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  n <- nrow(data) / 2

  l_index <- lapply(1:n, function(x) (1:(2 * n))[-c(x, (x + n))])
  u <- vapply(l_index, function(x) {
    stat_fun(data = data, ins = x, grouping = grouping, ...)
  },
  numeric(1))

  theta_hat <- stat_fun(data = data, ins = 1:(2 * n), grouping = grouping, ...)
  pseudo_values <- n * theta_hat - (n - 1) * u
  theta_jack <- mean(pseudo_values)
  jack_se <- sqrt(sum((pseudo_values - theta_jack)^2) / (n * (n - 1)))
  jack_bias <- (n - 1) * (theta_hat - mean(u))

  return(list(theta.hat = theta_hat,
              theta.jack = theta_jack,
              jack.se = jack_se,
              jack.bias = jack_bias,
              loo.values = u,
              pseudo.values = pseudo_values))
}
