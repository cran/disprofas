#' Model-independent multivariate confidence region (MIMCR) procedure
#'
#' The function \code{mimcr()} assesses the equivalence of highly variable
#' dissolution profiles. It does so by applying different methods proposed in
#' the literature, implementing the non-parametric \dQuote{Model-Independent
#' Multivariate Confidence Region} (MIMCR) procedure and the \dQuote{\eqn{T^2}
#' test for equivalence} of dissolution data as proposed by Hoffelder (2016).
#'
#' @param data A data frame with the dissolution profile data in wide format.
#' @param tcol A vector of indices that specifies the columns in \code{data}
#'   which contain the \% release values. The length of \code{tcol} must be
#'   two or longer.
#' @param grouping A character string that specifies the column in \code{data}
#'   that contains the group names (i.e. a factorial variable, e.g., for the
#'   differentiation of batches or formulations of a drug product).
#' @param fit_n_obs A logical value that indicates whether the number of rows
#'   per level in the column specified by the \code{grouping} parameter should
#'   be adjusted to be equal given that they are not equal. The default is
#'   \code{FALSE} because for this type of analysis each group should have the
#'   same number of observations. If \code{fit_n_obs} is \code{TRUE}, redundant
#'   observations from the level with more observations are dropped, i.e. only
#'   the observations \code{1:n} (n: number of observations of the level with
#'   the fewer observations) will be used for the comparison of the two groups.
#' @param mtad A numeric value that specifies the \dQuote{maximum tolerable
#'   average difference} (MTAD) of the profiles of two formulations at all time
#'   points (in \%). The default value is \code{10}. It determines the size of
#'   the similarity limit \eqn{\bm{d}_g}{d_g} (see the details section for more
#'   information).
#' @param signif A positive numeric value between \code{0} and \code{1} that
#'   specifies the significance level for the calculation of the
#'   \dQuote{Confidence Region} (CR). The coverage of CR is
#'   \eqn{(1 - signif) 100}\%. The default value is \code{0.05}.
#' @param max_trial A positive integer that specifies the maximum number of
#'   Newton-Raphson search rounds to be performed.
#' @param bounds A numeric vector of the form \code{c(lower, upper)} that
#'   specifies the \dQuote{lower} and \dQuote{upper} limits, respectively, for
#'   the \% drug release. The default is \code{c(1, 85)}. Mean \% release values
#'   of any of the two groups being compared that are smaller than or equal to
#'   the lower bound are ignored and only the first mean \% release value that
#'   is greater than or equal to the upper bound is included while all the
#'   subsequent values are ignored.
#' @param tol A non-negative numeric that specifies the accepted minimal
#'   difference between two consecutive search rounds.
#' @inheritParams bootstrap_f2
#'
#' @details The function \code{mimcr()} assesses the equivalence of highly
#' variable dissolution profiles by aid of a \dQuote{Model-Independent
#' Multivariate Confidence Region} (MIMCR) procedure as proposed by Tsong et al.
#' (1996) and by aid of a \dQuote{T2 test for equivalence} as proposed by
#' Hoffelder (2016).
#'
#' For details see the sections \dQuote{Comparison of highly variable
#' dissolution profiles}, \dQuote{Similarity limits in terms of MSD} and
#' \dQuote{T2 test for equivalence} below.
#'
#' @section Comparison of highly variable dissolution profiles:
#' When comparing the dissolution data of a post-approval change product and a
#' reference approval product, the goal is to assess the similarity between the
#' mean dissolution values at the observed sample time points. A widely used
#' method is the \eqn{f_2} method that was introduced by Moore & Flanner (1996).
#' Similarity testing criteria based on \eqn{f_2} can be found in several FDA
#' guidelines and in the guideline of the European Medicines Agency (EMA)
#' \dQuote{On the investigation of bioequivalence} (EMA 2010).
#'
#' In situations where within-batch variation is greater than 15\%, FDA
#' guidelines recommend use of a multivariate confidence interval as an
#' alternative to the \eqn{f_2} method. This can be done using the following
#' stepwise procedure:
#' \enumerate{
#' \item Establish a similarity limit in terms of \dQuote{Multivariate
#'   Statistical Distance} (MSD) based on inter-batch differences in \% drug
#'   release from reference (standard approved) formulations, i.e. the
#'   so-called \dQuote{Equivalence Margin} (EM).
#' \item Calculate the MSD between test and reference mean dissolutions.
#' \item Estimate the 90\% confidence interval (CI) of the true MSD as
#'   determined in step 2.
#' \item Compare the upper limit of the 90\% CI with the similarity limit
#'   determined in step 1. The test formulation is declared to be similar to
#'   the reference formulation if the upper limit of the 90\% CI is less than
#'   or equal to the similarity limit.
#' }
#'
#' @section Similarity limits in terms of MSD:
#' For the calculation of the \dQuote{Multivariate Statistical Distance} (MSD),
#' the procedure proposed by Tsong et al. (1996) can be considered as
#' well-accepted method that is actually recommended by the FDA. According
#' to this method, a multivariate statistical distance, called Mahalanobis
#' distance, is used to measure the difference between two multivariate means.
#' This distance measure is calculated as
#'
#' \deqn{D_M = \sqrt{ \left( \bm{x}_T - \bm{x}_R \right)^{\top}
#'   \bm{S}_{pooled}^{-1} \left( \bm{x}_T - \bm{x}_R \right)} ,}{%
#'   D_M = sqrt((x_T - x_R)^{\top} S_{pooled}^{-1} (x_T - x_R)) ,}
#'
#' where \eqn{\bm{S}_{pooled}} is the sample variance-covariance matrix pooled
#' across the comparative groups, \eqn{\bm{x}_T}{x_T} and \eqn{\bm{x}_R}{x_R}
#' are the vectors of the sample means for the test (\eqn{T}) and reference
#' (\eqn{R}) profiles, and \eqn{\bm{S}_T}{S_T} and \eqn{\bm{S}_R}{x_R} are the
#' variance-covariance matrices of the test and reference profiles. The pooled
#' variance-covariance matrix \eqn{\bm{S}_{pooled}}{S_{pooled}} is calculated
#' by
#'
#' \deqn{\bm{S}_{pooled} = \frac{(n_R - 1) \bm{S}_R + (n_T - 1) \bm{S}_T}{%
#'   n_R + n_T - 2} .}{S_{pooled} = ((n_R - 1) S_R + (n_T - 1) S_T) /
#'   (n_R + n_T - 2) .}
#'
#' In order to determine the similarity limits in terms of the MSD, i.e. the
#' Mahalanobis distance between the two multivariate means of the dissolution
#' profiles of the formulations to be compared, Tsong et al. (1996) proposed
#' using the equation
#'
#' \deqn{D_M^{max} = \sqrt{ \bm{d}_g^{\top} \bm{S}_{pooled}^{-1} \bm{d}_g} ,}{%
#'   D_M^{max} = sqrt(d_g^{\top} S_{pooled}^{-1} d_g) ,}
#'
#' where \eqn{\bm{d}_g}{d_g} is a \eqn{1 \times p}{1 x p} vector with all
#' \eqn{p} elements equal to an empirically defined limit \eqn{\bm{d}_g}{d_g},
#' e.g., \eqn{15}\%, for the maximum tolerable difference at all time points,
#' and \eqn{p} is the number of sampling points. By assuming that the data
#' follow a multivariate normal distribution, the 90\% confidence region
#' (\eqn{\textit{CR}}) bounds for the true difference between the mean vectors,
#' \eqn{\bm{\mu}_T - \bm{\mu}_R}{\mu_T - \mu_R}, can be computed for the
#' resultant vector \eqn{\bm{\mu}}{\mu} to satisfy the following condition:
#'
#' \deqn{\bm{\textit{CR}} = K \left( \bm{\mu} - \left( \bm{x}_T -
#'   \bm{x}_R \right) \right)^{\top} \bm{S}_{pooled}^{-1} \left( \bm{\mu} -
#'   \left( \bm{x}_T - \bm{x}_R \right) \right) \leq
#'   F_{p, n_T + n_R - p - 1, 0.9} ,}{%
#'   CR = sqrt((\mu - (x_T - x_R))^{\top} S_{pooled}^{-1} (\mu - (x_T - x_R)))
#'   \leq F_{p, n_T + n_R - p - 1, 0.9} ,}
#'
#' where \eqn{K} is the scaling factor that is calculated as
#'
#' \deqn{K = \frac{n_T n_R}{n_T + n_R} \; \frac{n_T + n_R - p - 1}{
#'   \left( n_T + n_R - 2 \right) p} ,}{%
#'   (n_T n_R) / (n_T + n_R) (n_T + n_R - p - 1) / ((n_T + n_R - 2) p) ,}
#'
#' and \eqn{F_{p, n_T + n_R - p - 1, 0.9}} is the \eqn{90^{th}} percentile of
#' the \eqn{F} distribution with degrees of freedom \eqn{p} and
#' \eqn{n_T + n_R - p - 1}, where \eqn{n_T} and \eqn{n_R} are the number of
#' observations of the reference and the test group, respectively, and \eqn{p}
#' is the number of sampling or time points, as mentioned already. It is
#' obvious that \eqn{(n_T + n_R)} must be greater than \eqn{(p + 1)}. The
#' formula for \eqn{\textit{CR}} gives a \eqn{p}-variate 90\% confidence region
#' for the possible true differences. \cr
#'
#' @section T2 test for equivalence:
#' Based on the distance measure for profile comparison that was suggested by
#' Tsong et al. (1996), i.e. the Mahalanobis distance, Hoffelder (2016) proposed
#' a statistical equivalence procedure for that distance, the so-called
#' \eqn{T^2} test for equivalence (T2EQ). It is used to demonstrate that the
#' Mahalanobis distance between reference and test group dissolution profiles
#' is smaller than the \dQuote{Equivalence Margin} (EM). Decision in favour of
#' equivalence is taken if the \eqn{p} value of this test statistic is smaller
#' than the pre-specified significance level \eqn{\alpha}, i.e. if
#' \eqn{p < \alpha}. The \eqn{p} value is calculated by aid of the formula
#'
#' \deqn{p = F_{p, n_T + n_R - p - 1, ncp, \alpha} \;
#'   \frac{n_T + n_R - p - 1}{(n_T + n_R - 2) p} T^2 ,}{%
#'   p = F_{p, n_T + n_R - p - 1, ncp, \alpha}
#'   (n_T + n_R - p - 1) / ((n_T + n_R - 2) p) \; T^2 ,}
#'
#' where \eqn{\alpha} is the significance level and \eqn{ncp} is the so-called
#' \dQuote{\emph{non-centrality parameter}} that is calculated by
#'
#' \deqn{\frac{n_T n_R}{n_T + n_R} \left( D_M^{max} \right)^2 .}{%
#'   (n_T n_R) / (n_T + n_R) (D_M^{max})^2 .}
#'
#' The test statistic being used is Hotelling's two-sample \eqn{T^2} test that
#' is given as
#'
#' \deqn{T^2 = \frac{n_T n_R}{n_T + n_R} \left( \bm{x}_T - \bm{x}_R
#'   \right)^{\top} \bm{S}_{pooled}^{-1} \left( \bm{x}_T - \bm{x}_R \right) .}{%
#'   (n_T n_R) / (n_T + n_R) (x_T - x_R)^{\top} S_{pooled}^{-1} (x_T - x_R) .}
#'
#' As mentioned in paragraph \dQuote{Similarity limits in terms of MSD},
#' \eqn{\bm{d}_g}{d_g} is a \eqn{1 \times p}{1 x p} vector with all \eqn{p}
#' elements equal to an empirically defined limit \eqn{d_g}. Thus, the
#' components of the vector \eqn{\bm{d}_g}{d_g} can be interpreted as upper
#' bound for a kind of \dQuote{\emph{average}} allowed difference between test
#' and reference profiles, the \dQuote{\emph{global similarity limit}}.
#' Since the EMA requires that \dQuote{similarity acceptance limits should be
#' pre-defined and justified and not be greater than a 10\% difference}, it is
#' recommended to use 10\%, not 15\% as proposed by Tsong et al. (1996), for
#' the maximum tolerable difference at all time points.
#'
#' @return An object of class \sQuote{\code{mimcr}} is returned, containing
#' the following list elements:
#' \item{Similarity}{Conclusion concerning similarity.}
#' \item{Parameters}{Parameters calculated during the assessment.}
#' \item{NR.CI}{List with results from the Newton-Raphson (NR) search.}
#' \item{Profile.TP}{A named numeric vector of the columns in \code{data}
#'   specified by \code{tcol}. Given that the column names contain extractable
#'   numeric information, e.g., the testing time points of the dissolution
#'   profile, it contains the corresponding numeric values. Elements where no
#'   numeric information could be extracted are \code{NA}.}
#'
#' The \code{Parameters} element contains the following information:
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
#' \item{Obs.L}{Observed lower limit (Tsong's procedure).}
#' \item{Obs.U}{Observed upper limit (Tsong's procedure).}
#'
#' The \code{NR.CI} element contains the following information:
#' \item{CI}{A matrix of the points on the \eqn{\textit{CR}} bounds for each
#'   time point.}
#' \item{converged}{A logical that indicates whether the NR algorithm converged
#'   or not.}
#' \item{points.on.crb}{A logical that indicates whether the points that were
#'   found by the NR algorithm sit on the confidence region boundary or not,
#'   i.e. whether the \eqn{T^2} statistic of the found data points, in relation
#'   to the mean difference, is equal to the critical \eqn{F} value.}
#' \item{n.trial}{Number of trials until convergence.}
#' \item{max.trial}{Maximal number of trials.}
#' \item{Warning}{A warning message, if applicable, or otherwise NULL.}
#' \item{Error}{An error message, if applicable, or otherwise NULL.}
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
#' Tsong, Y., Hammerstrom, T., Sathe, P.M., and Shah, V.P. Statistical
#' assessment of mean differences between two dissolution data sets.
#' \emph{Drug Inf J}. 1996; \strong{30}: 1105-1112.\cr
#' \doi{10.1177/009286159603000427}
#'
#' Tsong, Y., Hammerstrom, T., and Chen, J.J. Multipoint dissolution
#' specification and acceptance sampling rule based on profile modeling and
#' principal component analysis. \emph{J Biopharm Stat}. 1997; \strong{7}(3):
#' 423-439.\cr
#' \doi{10.1080/10543409708835198}
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
#' @seealso \code{\link{gep_by_nera}}, \code{\link{get_T2_two}},
#' \code{\link{get_T2_one}}, \code{\link{bootstrap_f2}}, \code{\link{mztia}}.
#'
#' @example man/examples/examples_mimcr.R
#'
#' @importFrom stats pf
#' @importFrom stats qf
#'
#' @export

mimcr <- function(data, tcol, grouping, fit_n_obs = FALSE, mtad = 10,
                  signif = 0.05, max_trial = 50, bounds = c(1, 85),
                  nsf = c(1, 2), tol = 1e-9) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.numeric(tcol) || length(tcol) < 2) {
    stop("The parameter tcol must be an integer vector of at least length 2.")
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
  if (!is.logical(fit_n_obs) || length(fit_n_obs) != 1) {
    stop("The parameter fit_n_obs must be a logical of length 1.")
  }
  if (mtad <= 0 || mtad > 50) {
    stop("Please specify mtad as (0, 50]")
  }
  if (signif <= 0 || signif > 1) {
    stop("Please specify signif as (0, 1]")
  }
  if (!is.numeric(max_trial) || length(max_trial) > 1) {
    stop("The parameter max_trial must be a positive integer of length 1.")
  }
  if (max_trial != as.integer(max_trial)) {
    stop("The parameter max_trial must be a positive integer of length 1.")
  }
  if (max_trial < 0) {
    stop("The parameter max_trial must be a positive integer of length 1.")
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
  if (!is.numeric(tol) || length(tol) > 1) {
    stop("The parameter tol must be a non-negative numeric value of length 1.")
  }
  if (tol < 0) {
    stop("The parameter tol must be a non-negative numeric value of length 1.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of data

  data <- droplevels(data)

  if (nlevels(data[, grouping]) != 2) {
    stop("The number of levels in column ", grouping, " differs from 2.")
  }

  # Generation of logical vector representing the reference and test group
  b1 <- make_grouping(data = data, grouping = grouping)

  # Check if the two groups have the same number of observations. If  not so,
  # adjust the data frame in such a way that both groups to be compared will
  # have the same number of observations and that the number of observations
  # per group corresponds to the number of observations of the group with the
  # fewer observations (mno: minimal number of observations).

  if (fit_n_obs && sum(b1) != sum(!b1)) {
    warning("Rows from the group with redundant observations are dropped.")

    mno <- min(sum(b1), sum(!b1))

    slctn1 <- 1:mno + (which(b1)[1] - 1)
    slctn2 <- 1:mno + (which(!b1)[1] - 1)

    # Together with the data adjustment the b1 vector must be reset.
    data <- rbind(data[slctn1, ], data[slctn2, ])
    b1 <- make_grouping(data = data, grouping = grouping)
  }

  if (length(data[b1, grouping]) != length(data[!b1, grouping])) {
    stop("The treatments to be tested must have the same number of rows.")
  }

  # <-><-><->

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
                            use_ema = "no", bounds = bounds, nsf = nsf)

  if (sum(ok) < 3) {
    warning("The profiles should comprise a minimum of 3 time points.\n",
            "  The actual profiles comprise ", sum(ok), " points only.")
  }

  # <-><-><->
  # Extraction of information on the time points
  time_points <- get_time_points(svec = colnames(data)[tcol])[ok]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of MSD and similarity assessment

  # Hotelling's T2 statistics
  l_hs <- get_T2_two(m1 = as.matrix(data[b1, tcol[ok]]),
                     m2 = as.matrix(data[!b1, tcol[ok]]),
                     signif = signif)

  # Similarity limit and critical F values
  t_sl <- get_sim_lim(mtad, l_hs)

  # Similarity conclusion based on Hoffelder's p value
  if (t_sl["p.F.Hoffelder"] < signif) {
    conclusion_hoffelder <- "Similar"
  } else {
    conclusion_hoffelder <- "Dissimilar"
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of points on the confidence region boundary (CRB) according
  # to Tsong 1996

  l_nera <- try_get_model(
    gep_by_nera(n_p = as.numeric(l_hs[["Parameters"]]["df1"]),
                kk = as.numeric(l_hs[["Parameters"]]["K"]),
                mean_diff = l_hs[["means"]][["mean.diff"]],
                m_vc = l_hs[["S.pool"]],
                ff_crit = as.numeric(l_hs[["Parameters"]]["F.crit"]),
                y = rep(1, times = l_hs[["Parameters"]]["df1"] + 1),
                max_trial = max_trial,
                tol = tol))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check if the estimation by gep_by_nera() was successful and if the points
  # that were found sit on the CRB

  # Extend t_res vector for the reporting of the results
  t_res <- c(t_sl, NA, NA)
  names(t_res)[(length(t_res) - 1):length(t_res)] <- c("Obs.L", "Obs.U")

  if (!is.null(l_nera[["Error"]]) || !is.null(l_nera[["Warning"]])) {
      nr_ci <- cbind(LCL = rep(NA, times = t_sl["df1"]),
                     UCL = rep(NA, times = t_sl["df1"]))
      rownames(nr_ci) <- colnames(data[, tcol[ok]])

      # Similarity conclusion based on Tsong's D_crit
      conclusion_tsong <- NA
    } else {
      # If the found points are not located on the CRB
      l_nera[["Model"]] <- check_point_location(lpt = l_nera[["Model"]],
                                                lhs = l_hs)

      if (!l_nera[["Model"]][["points.on.crb"]]) {
        nr_ci <- cbind(LCL = rep(NA, times = t_sl["df1"]),
                       UCL = rep(NA, times = t_sl["df1"]))
        rownames(nr_ci) <- colnames(data[, tcol[ok]])

        warning("The points found by the Newton-Raphson search are not\n",
                "  located on the confidence region boundary (CRB).")

        # Similarity conclusion based on Tsong's D_crit
        conclusion_tsong <- NA
      } else {
        # If it has been confirmed that the found data points are located on
        # the CRB, the corresponding Mahalanobis distances are calculated.
        # Then it is checked if the longer distance is smaller than the global
        # similarity limit.
        y_b1 <- l_nera[["Model"]][["points"]]
        md_1 <- sqrt(t(y_b1[1:t_sl["df1"]]) %*% solve(l_hs[["S.pool"]]) %*%
                       y_b1[1:t_sl["df1"]])

        # The points at the ellipse's opposite side are obtained by subtraction.
        y_b2 <- l_hs[["means"]][["mean.diff"]] +
          (l_hs[["means"]][["mean.diff"]] - y_b1[1:t_sl["df1"]])
        md_2 <- sqrt(t(y_b2[1:t_sl["df1"]]) %*% solve(l_hs[["S.pool"]]) %*%
                      y_b2[1:t_sl["df1"]])

        t_res[length(t_res) - 1] <- min(md_1, md_2)
        t_res[length(t_res)] <- max(md_1, md_2)

        # Similarity conclusion based on Tsong's similarity limit
        if (t_res[length(t_res)] < t_sl["Sim.Limit"]) {
          conclusion_tsong <- "Similar"
        } else {
          conclusion_tsong <- "Dissimilar"
        }

        if (md_1 < md_2) {
          nr_ci <- cbind(LCL = y_b1[1:t_sl["df1"]], UCL = y_b2[1:t_sl["df1"]])
          rownames(nr_ci) <- colnames(data[, tcol[ok]])
        } else {
          nr_ci <- cbind(LCL = y_b2[1:t_sl["df1"]], UCL = y_b1[1:t_sl["df1"]])
          rownames(nr_ci) <- colnames(data[, tcol[ok]])
        }
      }
    }

  l_nr <- list(CI = NA,
               converged = NA,
               points.on.crb = NA,
               n.trial = NA,
               max.trial = max_trial,
               Warning = NA,
               Error = NA)

  if (!is.null(l_nera[["Error"]])) {
    l_nr[["CI"]] <- nr_ci
    l_nr[["Error"]] <- l_nera[["Error"]]
  } else {
    l_nr[["CI"]] <- nr_ci
    l_nr[["converged"]] <- l_nera[["Model"]][["converged"]]
    l_nr[["points.on.crb"]] <- l_nera[["Model"]][["points.on.crb"]]
    l_nr[["n.trial"]] <- l_nera[["Model"]][["n.trial"]]
    if (!is.null(l_nera[["Warning"]])) l_nr[["Warning"]] <- l_nera[["Warning"]]
  }

  conclusions <- c(conclusion_tsong, conclusion_hoffelder)
  names(conclusions) <- c("Tsong", "Hoffelder")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compilation of results

  structure(list(Similarity = conclusions,
                 Parameters = t_res,
                 NR.CI = l_nr,
                 Profile.TP = time_points),
            class = "mimcr")
}
