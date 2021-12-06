#' Profile portion determination
#'
#' The function \code{get_profile_portion()} determines, depending on the value
#' of \code{use_EMA}, which part of the profile will be used for the similarity
#' assessment (EMA: European Medicines Agency).
#'
#' @param data A data frame with the dissolution profile data in wide format.
#' @param tcol A vector of indices specifying the columns in \code{data} that
#'   contain the \% release values. The length of \code{tcol} must be two or
#'   longer.
#' @param groups A logical vector specifying different groups.
#' @inheritParams f2
#'
#' @details The function \code{get_profile_portion()} determines which part of
#' a dissolution profile is used for comparison based on the recommendations of
#' the European Medicines Agency's guideline \dQuote{On the investigation of
#' bioequivalence}. It says that profile similarity testing and any conclusions
#' drawn from the results can be considered valid only if the dissolution
#' profile has been satisfactorily characterised using a sufficient number of
#' time points. For immediate release formulations comparison at 15 minutes is
#' essential to know if complete dissolution is reached before gastric emptying.
#' Where more than 85\% of the drug is dissolved within 15 minutes, dissolution
#' profiles may be accepted as similar without further mathematical evaluation.
#' In case more than 85\% is not dissolved at 15 minutes but within 30 minutes,
#' at least three time points are required: the first time point before 15
#' minutes, the second at 15 minutes, and the third time point when the release
#' is close to 85\%. For modified release products, the advice given in the
#' relevant guidance should be followed. Dissolution similarity may be
#' determined using the \eqn{f_2} statistic as follows:
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
#' For both the reference and the test formulations, percent dissolution should
#' be determined. The evaluation of the similarity factor is based on the
#' following conditions (called \dQuote{EMA Rules}, from the European Medicines
#' Agency (EMA) guideline \dQuote{On the investigation of bioequivalence}):
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
#' An \eqn{f_2} value between 50 and 100 suggests that the two dissolution
#' profiles are similar.
#'
#' @return The function returns a logical vector defining the appropriate
#' profile portion.
#'
#' @references
#' European Medicines Agency (EMA), Committee for Medicinal Products for
#' Human Use (CHMP). Guideline on the Investigation of Bioequivalence. 2010;
#' CPMP/EWP/QWP/1401/98 Rev. 1.\cr
#' \url{https://www.ema.europa.eu/en/documents/scientific-guideline/
#' guideline-investigation-bioequivalence-rev1_en.pdf}
#'
#' @keywords internal

get_profile_portion <- function(data, tcol, groups, use_EMA = "yes",
                                bounds = c(1, 85)) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.numeric(tcol) | length(tcol) < 2) {
    stop("The parameter tcol must be an integer vector of at least length 2.")
  }
  if (!isTRUE(all.equal(tcol, as.integer(tcol)))) {
    stop("The parameter tcol must be an integer vector.")
  }
  if (min(tcol) < 1 | max(tcol) > ncol(data)) {
    stop("Some columns specified by tcol were not found in data frame.")
  }
  if (sum(grepl("\\d", colnames(data[, tcol]))) < length(tcol)) {
    stop("Some names of columns specified by tcol ",
         "do not contain numeric information.")
  }
  if (sum(vapply(data[, tcol], is.numeric, logical(1))) != length(tcol)) {
    stop("Some columns specified by tcol are not numeric.")
  }
  if (!is.logical(groups) | length(groups) != nrow(data)) {
    stop("The parameter groups must be a logical vector of length nrow(data).")
  }
  if (!(use_EMA %in% c("yes", "no", "ignore"))) {
    stop("Please specify use_EMA either as \"yes\" or \"no\" or \"ignore\".")
  }
  if (!is.numeric(bounds) | length(bounds) != 2) {
    stop("The paramter bounds must be a numeric vector of length 2.")
  }
  if (bounds[1] > bounds[2]) {
    stop("Please specify bounds in the form c(lower limit, upper limit).")
  }
  if (bounds[1] < 0 | bounds[2] > 100) {
    stop("Please specify bounds in the range [0, 100].")
  }

  n <- length(tcol)
  b1 <- groups

  switch(use_EMA, "yes" = {
    m_results <- matrix(NA, ncol = 6, nrow = n)
    colnames(m_results) <- c("mean.1", "mean.2", "sd.1", "sd.2", "CV.1", "CV.2")
    rownames(m_results) <- colnames(data)[tcol]

    m_results[, 1] <- apply(data[b1, tcol], MARGIN = 2, FUN = mean)
    m_results[, 2] <- apply(data[!b1, tcol], MARGIN = 2, FUN = mean)
    m_results[, 3] <- apply(data[b1, tcol], MARGIN = 2, FUN = sd)
    m_results[, 4] <- apply(data[!b1, tcol], MARGIN = 2, FUN = sd)
    m_results[, 5] <- m_results[, 3] / m_results[, 1] * 100
    m_results[, 6] <- m_results[, 4] / m_results[, 2] * 100

    m_tests <- matrix(NA, ncol = 5, nrow = n)
    colnames(m_tests) <-
      c("> 0%", "< 85%", "< 20%", "< 10%", "< 20% & < 10%")
    rownames(m_tests) <- colnames(data)[tcol]

    # Tests and Settings
    # 1) Tests for points equal to 0% to exclude them (column 1).
    # 2a) Tests for points bigger than 85%.
    # 2b) Includes the first point > 85%.
    # 2c) Stores the result in column 2.
    # 3) Tests for points with CV bigger than 20% (column 3).
    # 4) Tests for points with CV bigger than 10% (column 4).
    # 5) Set NA entries (division through 0) to FALSE (column 3).
    # 6) Set NA entries (division through 0) to FALSE (column 4).
    # 7a) Copies the result from column 3 to column 5.
    # 7b) Makes sure that no more than one point preceding the first
    #     "< 10% point" is < 20%.
    # 8) Combines all the test into the final result.
    m_tests[, 1] <- m_results[, 1] > 0 & m_results[, 2] > 0
    tmp <- m_results[, 1] > 85 | m_results[, 2] > 85
    tmp[as.numeric(which(tmp)[1])] <- FALSE
    m_tests[, 2] <- !tmp
    m_tests[, 3] <- m_results[, 5] < 20 & m_results[, 6] < 20
    m_tests[, 4] <- m_results[, 5] < 10 & m_results[, 6] < 10
    m_tests[is.na(m_tests[, 3]), 2] <- FALSE
    m_tests[is.na(m_tests[, 4]), 3] <- FALSE
    m_tests[, 5] <- m_tests[, 3]

    if (which(m_tests[, 4])[1] > 2) {
      m_tests[1:(as.numeric(which(m_tests[, 4]))[1] - 2), 5] <- FALSE
    }

    ok <- (m_tests[, 4] | m_tests[, 5]) & (m_tests[, 1] & m_tests[, 2])

    # Check for time points regarded as TRUE in the part to the right of the
    # first time point with failed acceptance criteria after the part of the
    # profile that has been identified as acceptable.
    tmp <- which(!ok[as.numeric(which(ok))[1]:length(ok)])
    if (length(tmp) > 0) {
      tmp2 <- as.numeric(tmp[1] + which(ok)[1])

      if (tmp2 < length(ok)) {
        ok[tmp2:length(ok)] <- FALSE
      }
    }
  }, "no" = {
    m_results <- matrix(NA, ncol = 2, nrow = n)
    colnames(m_results) <- c("mean.1", "mean.2")
    rownames(m_results) <- colnames(data)[tcol]

    m_results[, 1] <- apply(data[b1, tcol], MARGIN = 2, FUN = mean)
    m_results[, 2] <- apply(data[!b1, tcol], MARGIN = 2, FUN = mean)

    m_tests <- matrix(NA, ncol = 2, nrow = n)
    colnames(m_tests) <- c("< upper.bound", "> lower.bound")
    rownames(m_tests) <- colnames(data)[tcol]

    # Tests and Settings
    # 1a) Tests for points bigger than bounds[2]
    # 1b) Includes the first point > 85%.
    # 1c) Stores the result of 1a) in column 1.
    # 2) Tests for points smaller than bounds[1] to exclude them (column 2).
    # 3) Combines tests 1) and 2) into the final result.
    tmp <- m_results[, 1] > bounds[2] | m_results[, 2] > bounds[2]
    tmp[as.numeric(which(tmp)[1])] <- FALSE
    m_tests[, 1] <- !tmp
    m_tests[, 2] <- m_results[, 1] > bounds[1] & m_results[, 2] > bounds[1]

    ok <- m_tests[, 1] & m_tests[, 2]
  }, "ignore" = {
    ok <- rep(TRUE, n)
  })

  return(ok)
}

#' Get time points
#'
#' The function \code{get_time_points()} extracts the numeric information from
#' a vector of character strings, if available.
#'
#' @param svec A vector of character strings.
#'
#' @details The function expects a vector of character strings that contain
#' numeric information. If the strings contain extractable numeric information
#' a named numeric vector is returned, where the names are derived from the
#' strings provided by \code{svec}. For example, from the vector
#' \code{c("t_0", "t_5", "t_10")} the named numeric vector \code{(0, 5, 10)}
#' is returned, where the names correspond to the original string. If a string
#' does not contain any numeric information \code{NA} is returned.
#'
#' @return A vector of the same length as \code{svec} with the extracted
#' numbers as numeric values.
#'
#' @keywords internal

get_time_points <- function(svec) {
  if (!is.character(svec)) {
    stop("The parameter svec must be string or string vector.")
  }

  pattern <- "(?>-)*[[:digit:]]+\\.{0,1}[[:digit:]]{0,1}"

  res <- as.numeric(gsub("[^0-9]", "", svec))
  names(res) <- svec

  where_num <- grepl(pattern, svec, perl = TRUE)
  num <- as.numeric(regmatches(svec, regexpr(pattern, svec, perl = TRUE)))

  if (length(num) > 0) {
    res[where_num] <- num
  }

  return(res)
}

#' Grouping
#'
#' The function \code{make_grouping()} makes the grouping according to the
#' \code{grouping} column.
#'
#' @param data A data frame with the dissolution profile data in wide format
#'   and a column for the distinction of the groups to be compared.
#' @inheritParams f2
#'
#' @details If one of the two levels of the \code{grouping} column is named
#' \dQuote{references} or \dQuote{References} or some abbreviation thereof
#' (in the extreme case just \dQuote{r}) this level will be used as reference
#' level. Otherwise the first level of the \code{grouping} column (according to
#' the level sorting of the column) will be assumed representing the reference
#' group.
#'
#' @return A logical vector of length \code{nrow(data)} where \code{TRUE}
#' represents the reference and \code{FALSE} represents the test group.
#'
#' @keywords internal

make_grouping <- function(data, grouping) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.character(grouping)) {
    stop("The parameter grouping must be string.")
  }
  if (!(grouping %in% colnames(data))) {
    stop("The grouping variable was not found in the provided data frame.")
  }
  if (!is.factor(data[, grouping])) {
    stop("The column in data specified by grouping must be a factor.")
  }

  # <-><-><->

  b1 <- tolower(substr(x = data[, grouping], start = 1, stop = 1)) %in% "r"

  if (sum(b1) == 0 | sum(b1) == nrow(data)) {
    b1 <- data[, grouping] == levels(data[, grouping])[1]
  }

  return(b1)
}

#' Balance observations
#'
#' The function \code{balance_observations()} balances the number of
#' observations of two groups.
#'
#' @param n_obs An integer specifying the minimal number of observations each
#'   group should have.
#' @inheritParams get_profile_portion
#'
#' @details First, the largest common value between \code{n_obs} and the number
#' of observations of the two groups specified by \code{groups} is sought. Then,
#' the number of observations of the two groups are extended according to the
#' value found. Extension means that the maximal possible number of
#' observations is duplicated in order to obtain the required number of
#' observations. Thus, in the data frame that is returned the two groups will
#' have the same number of observations. Either the number of observations of
#' both groups is extended (to match the number of \code{n_obs}), or, if
#' \code{n_obs} and the number of observations of one of the two groups is
#' equal, only the number of observations of one of the two groups.
#'
#' @return The original data frame extended by the observations necessary to
#' have a balanced number of observations between the two groups.
#'
#' @keywords internal

balance_observations <- function(data, groups, n_obs) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.logical(groups) | length(groups) != nrow(data)) {
    stop("The parameter groups must be a logical vector of length nrow(data).")
  }
  if (!isTRUE(all.equal(n_obs, as.integer(n_obs)))) {
    stop("The parameter n_obs must be an integer.")
  }

  b1 <- groups
  lcv <- max(n_obs, sum(b1), sum(!b1))
  slctn1 <- slctn2 <- numeric()
  index1 <- which(b1)
  index2 <- which(!b1)

  if (sum(b1) < lcv) {
    if (lcv %% sum(b1) == 0) {
      slctn1 <- index1[rep(1:sum(b1), lcv / sum(b1))]
    } else {
      slctn1 <- index1[c(rep(1:sum(b1), floor(lcv / sum(b1))),
                         1:(lcv %% sum(b1)))]
    }
  } else {
    slctn1 <- index1[1:lcv]
  }

  if (sum(!b1) < lcv) {
    if (lcv %% sum(!b1) == 0) {
      slctn2 <- index2[rep(1:sum(!b1), lcv / sum(!b1))]
    } else {
      slctn2 <- index2[c(rep(1:sum(!b1), floor(lcv / sum(!b1))),
                         1:(lcv %% sum(!b1)))]
    }
  } else {
    slctn2 <- index2[1:lcv]
  }

  data <- rbind(data[slctn1, ], data[slctn2, ])

  return(data)
}

#' Randomisation of individual data points
#'
#' The function \code{rand_indiv_points()} samples individual data points of
#' each profile.
#'
#' @param data A data frame with the dissolution profile data in wide format.
#' @param mle A list of further arguments required for the generation of a
#'   randomised data frame. The first element of \code{mle} is a numeric value
#'   of the number of profiles per batch and the second element is a vector of
#'   indices of the columns containing the profile data.
#'
#' @details The function \code{rand_indiv_points()} samples individual data
#' points of each profile. The first element of \code{mle} specifies the
#' number of indices per group (of two groups), i.e. the number of profiles
#' to take into account from each group. The second element of \code{mle}
#' specifies the columns in the data frame that represent the dissolution
#' profiles, i.e. the columns with the \% release data. The data points of
#' each group and column are randomised.
#'
#' @return A data frame representing a randomised version of the data frame
#' that has been handed over via the \code{data} parameter.
#'
#' @seealso \code{\link{bootstrap_f2}}.
#'
#' @keywords internal

rand_indiv_points <- function(data, mle) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!inherits(mle, "list") | length(mle) != 2) {
    stop("The parameter mle must be a list of length 2.")
  }
  if (!is.numeric(mle[[1]]) | length(mle[[1]]) != 1) {
    stop("The first element of mle must be an integer value.")
  }
  if (!isTRUE(all.equal(mle[[1]], as.integer(mle[[1]])))) {
    stop("The first element of mle must be an integer value.")
  }
  if (nrow(data) != 2 * mle[[1]]) {
    stop("The first element of mle must be half of the number of rows in data.")
  }
  if (!is.numeric(mle[[2]]) | length(mle[[2]]) < 2) {
    stop("The second element of mle must be an integer vector of at least ",
         "length 2.")
  }
  if (!isTRUE(all.equal(mle[[2]], as.integer(mle[[2]])))) {
    stop("The second element of mle must be an integer vector.")
  }
  if (min(mle[[2]]) < 1 | max(mle[[2]]) > ncol(data)) {
    stop("Some columns specified by the second element of mle were not found ",
         "in data.")
  }
  if (sum(grepl("\\d", colnames(data[, mle[[2]]]))) < length(mle[[2]])) {
    stop("Some columns specified by the second element of mle do not contain ",
         "numeric information.")
  }

  # <-><-><-><->

  n <- mle[[1]]
  tcol <- mle[[2]]

  index_R <- matrix(sample.int(n = n, size = n * length(tcol), replace = TRUE),
                    ncol = length(tcol))
  index_T <- matrix(sample.int(n = n, size = n * length(tcol),
                               replace = TRUE) + n, ncol = length(tcol))
  im <- rbind(index_R, index_T)

  res <- data
  for (i in seq_along(tcol)) {
    res[1:(2 * n), tcol[i]] <- data[im[, i], tcol[i]]
  }

  return(res)
}

#' Points on confidence region (found by Newton-Raphson search)
#'
#' The function \code{gep_by_nera()} is a function for finding points on
#' confidence region (\eqn{CR}) bounds by aid of the \dQuote{Method of Lagrange
#' Multipliers} (MLM) and by \dQuote{Newton-Raphson} (nera) optimisation. The
#' multivariate confidence interval for profiles with four time points, e.g.,
#' is an \dQuote{ellipse} in four dimensions.
#'
#' @param n_p A positive integer specifying the number of (time) points
#'   \eqn{n_p}.
#' @param K A non-negative numeric value specifying the scaling factor \eqn{K}
#'   for the calculation of the Hotelling's \eqn{T^2} statistic.
#' @param mean_diff A vector of the mean differences between the dissolution
#'   profiles of the reference and the test batch. It must have the length
#'   specified by the parameter \eqn{n_p}.
#' @param S_pool The pooled variance-covariance matrix of the dissolution
#'   profiles of the reference and the test batch. It must have the dimension
#'   \eqn{n_p \times n_p}.
#' @param F_crit The critical \eqn{F} value (i.e. a non-negative numeric).
#' @param y A numeric vector of \eqn{y} values that serve as starting points
#'   for the Newton-Raphson search, i.e. values supposed to lie on or close to
#'   the confidence interval bounds. It must have a length of \eqn{n_p + 1}.
#' @inheritParams mimcr
#'
#' @details The function \code{gep_by_nera()} determines the points on the
#' \eqn{CR} bounds for each of the \eqn{n_p} time points. It does so by aid
#' of the \dQuote{Method of Lagrange Multipliers} (MLM) and by
#' \dQuote{Newton-Raphson} (nera) optimisation, as proposed by Margaret
#' Connolly (Connolly 2000).
#'
#' For more information, see the sections \dQuote{Comparison of highly variable
#' dissolution profiles} and \dQuote{Similarity limits in terms of MSD} below.
#'
#' @inheritSection mimcr Comparison of highly variable dissolution profiles
#'
#' @inheritSection mimcr Similarity limits in terms of MSD
#'
#' @return A list with the following elements is returned:
#' \item{points}{A matrix with one column and \eqn{n_p + 1} rows is returned,
#'   where rows \eqn{1} to \eqn{n_p} represent, for each time point, the points
#'   on the \eqn{CR} bounds. For symmetry reasons, the points on the opposite
#'   side are obtained by addition/subtraction. The last row in the matrix,
#'   with index \eqn{n_p + 1}, represents the \eqn{\lambda} parameter of the
#'   MLM, also known as \emph{lambda multiplier method}, that is used to
#'   optimise under constraint(s). The variable \eqn{\lambda} is thus called
#'   the \emph{Lagrange multiplier}.}
#' \item{converged}{A logical stating if the NR algorithm converged or not.}
#' \item{n.trial}{Number of trials until convergence.}
#' \item{max.trial}{Maximal number of trials.}
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
#' CPMP/EWP/QWP/1401/98 Rev. 1.\cr
#' \url{https://www.ema.europa.eu/en/documents/scientific-guideline/
#' guideline-investigation-bioequivalence-rev1_en.pdf}
#'
#' Moore, J.W., and Flanner, H.H. Mathematical comparison of curves with an
#' emphasis on \emph{in-vitro} dissolution profiles. \emph{Pharm Tech}. 1996;
#' \strong{20}(6): 64-74.
#'
#' Tsong, Y., Hammerstrom, T., Sathe, P.M., and Shah, V.P. Statistical
#' assessment of mean differences between two dissolution data sets.
#' \emph{Drug Inf J}. 1996; \strong{30}: 1105-1112.\cr
#' \doi{10.1177/009286159603000427}
#'
#' Connolly, M. SAS(R) IML Code to calculate an upper confidence limit for
#' multivariate statistical distance; 2000; Wyeth Lederle Vaccines, Pearl River,
#' NY.\cr
#' \url{https://analytics.ncsu.edu/sesug/2000/p-902.pdf}
#'
#' @seealso \code{\link{mimcr}}, \code{\link{bootstrap_f2}}.
#'
#' @example man/examples/examples_gep_by_nera.R
#'
#' @export

gep_by_nera <- function(n_p, K, mean_diff, S_pool, F_crit, y,
                        max_trial, tol) {
  if (!is.numeric(n_p) | length(n_p) > 1) {
    stop("The parameter n_p must be a positive integer.")
  }
  if (n_p != as.integer(n_p)) {
    stop("The parameter n_p must be a positive integer.")
  }
  if (n_p < 0) {
    stop("The parameter n_p must be a positive integer.")
  }
  if (!is.numeric(K) | length(K) > 1) {
    stop("The parameter K must be a non-negative numeric value of length 1.")
  }
  if (K < 0) {
    stop("The parameter K must be a non-negative numeric value of length 1.")
  }
  if (!is.numeric(mean_diff) | length(mean_diff) != n_p) {
    stop("The parameter mean_diff must be a numeric vector of length n_p.")
  }
  if (!is.matrix(S_pool)) {
    stop("The parameter S_pool must be a matrix of dimensions n_p x n_p.")
  }
  if (!isTRUE(all.equal(dim(S_pool), c(n_p, n_p)))) {
    stop("The parameter S_pool must be a matrix of dimensions n_p x n_p.")
  }
  if (!is.numeric(F_crit) | length(F_crit) > 1) {
    stop("The parameter F_crit must be a non-negative numeric value of ",
         "length 1.")
  }
  if (F_crit < 0) {
    stop("The parameter F_crit must be a non-negative numeric value of ",
         "length 1.")
  }
  if (!is.numeric(y) | length(y) != (n_p + 1)) {
    stop("The parameter y must be a numeric vector of length (n_p + 1).")
  }
  if (!is.numeric(max_trial) | length(max_trial) > 1) {
    stop("The parameter max_trial must be a positive integer of length 1.")
  }
  if (max_trial != as.integer(max_trial)) {
    stop("The parameter max_trial must be a positive integer of length 1.")
  }
  if (max_trial < 0) {
    stop("The parameter max_trial must be a positive integer of length 1.")
  }
  if (!is.numeric(tol) | length(tol) > 1) {
    stop("The parameter tol must be a non-negative numeric value of length 1.")
  }
  if (tol < 0) {
    stop("The parameter tol must be a non-negative numeric value of length 1.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of data

  m_j <- matrix(1, nrow = (n_p + 1), ncol = (n_p + 1))
  i <- 0

  repeat {
    t_val <- y[1:n_p]
    t_diff <- t_val - mean_diff
    lambda <- y[n_p + 1]

    # The first partial derivatives
    f_deriv1 <- 2 * solve(S_pool) %*% t_val - 2 * lambda *
      K * solve(S_pool) %*% t_diff
    g_deriv1 <- F_crit - K * t(t_diff) %*% solve(S_pool) %*% t_diff

    m_score1 <- c(f_deriv1, g_deriv1)

    # The second partial derivatives (Hessian matrix)
    f_deriv2 <- 2 * solve(S_pool) - 2 * lambda * K * solve(S_pool)
    g_deriv2 <- -2 * K * solve(S_pool) %*% t_diff

    m_score2 <- rbind(f_deriv2, c(g_deriv2))
    m_score2 <- cbind(m_score2, c(c(g_deriv2), 0))

    # Newton-Raphson algorithm for k-dimensional function optimisation
    y <- y - solve(m_score2) %*% m_score1
    i <- i + 1

    # Test
    if (sum(abs(t(m_j) %*% m_score1) < tol) > 0 | i >= max_trial) break
  }

  if (sum(abs(t(m_j) %*% m_score1) < tol) == 0) {
    warning("The Newton-Raphson search did not converge.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compilation of results

  return(list(points = y,
              converged = ifelse(i >= max_trial, FALSE, TRUE),
              n.trial = i,
              max.trial = max_trial))
}
