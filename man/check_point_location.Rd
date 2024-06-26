% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{check_point_location}
\alias{check_point_location}
\title{Check point location}
\usage{
check_point_location(lpt, lhs)
}
\arguments{
\item{lpt}{A list returned by the \code{\link{gep_by_nera}()} function.}

\item{lhs}{A list of the estimates of Hotelling's two-sample \eqn{T^2}
statistic for small samples as returned by the function
\code{\link{get_T2_two}()}.}
}
\value{
The function returns the list that was passed in via the \code{lpt}
parameter with a modified \code{points.on.crb} element, i.e. set as
\code{TRUE} if the points sit on the \eqn{\textit{CRB}} or \code{FALSE} if
they do not sit on the \eqn{\textit{CRB}}.
}
\description{
The function \code{check_point_location()} checks if points that were found
by the \code{\link{gep_by_nera}()} function sit on specified confidence
region bounds (\eqn{\textit{CRB}}) or not. This is necessary because the
points found by aid of the \dQuote{Method of Lagrange Multipliers} (MLM)
and \dQuote{Newton-Raphson} (nera) optimisation may not sit on the
\eqn{\textit{CRB}}.
}
\details{
The function \code{check_point_location()} checks if points that
were found by the \code{\link{gep_by_nera}()} function sit on specified
confidence region bounds (\eqn{\textit{CRB}}) or not. The
\code{\link{gep_by_nera}()} function determines the points on the
\eqn{\textit{CRB}} for each of the \eqn{n_p} time points or model parameters
by aid of the \dQuote{Method of Lagrange Multipliers} (MLM) and by
\dQuote{Newton-Raphson} (nera) optimisation, as proposed by Margaret
Connolly (Connolly 2000). However, since the points found may not sit on
the specified \eqn{\textit{CRB}}, it must be checked if the points returned
by the \code{\link{gep_by_nera}()} function do sit on the \eqn{\textit{CRB}}
or not.
}
\examples{
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
}
\references{
Tsong, Y., Hammerstrom, T., Sathe, P.M., and Shah, V.P. Statistical
assessment of mean differences between two dissolution data sets.
\emph{Drug Inf J}. 1996; \strong{30}: 1105-1112.\cr
\doi{10.1177/009286159603000427}

Connolly, M. SAS(R) IML Code to calculate an upper confidence limit for
multivariate statistical distance; 2000; Wyeth Lederle Vaccines, Pearl River,
NY.\cr
\url{https://analytics.ncsu.edu/sesug/2000/p-902.pdf}
}
\seealso{
\code{\link{mimcr}}, \code{\link{gep_by_nera}}.
}
