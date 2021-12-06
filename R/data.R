#' Dissolution data of a reference and a test batch
#'
#' A data set containing the dissolution data of one reference batch and one
#'   test batch of \eqn{n = 6} tablets each, i.e. the dissolution profiles of
#'   the \% drug release observed within a period of 120 minutes.
#'
#' @docType data
#'
#' @usage data(dip1)
#'
#' @format A data frame with 12 observations and 10 variables:
#' \describe{
#'   \item{type}{Factor with levels \code{R} (Reference) and \code{T} (Test)}
#'   \item{tablet}{Factor with levels \code{1} to \code{6} representing
#'     individual tablets}
#'   \item{t.5}{Numeric of the \% release at the 5 minutes testing point}
#'   \item{t.10}{Numeric of the \% release at the 10 minutes testing point}
#'   \item{t.15}{Numeric of the \% release at the 15 minutes testing point}
#'   \item{t.20}{Numeric of the \% release at the 20 minutes testing point}
#'   \item{t.30}{Numeric of the \% release at the 30 minutes testing point}
#'   \item{t.60}{Numeric of the \% release at the 60 minutes testing point}
#'   \item{t.90}{Numeric of the \% release at the 90 minutes testing point}
#'   \item{t.120}{Numeric of the \% release at the 120 minutes testing point}
#' }
#'
#' @references
#' Tsong, Y., Hammerstrom, T., Sathe, P.M., and Shah, V.P. Statistical
#' assessment of mean differences between two dissolution data sets.
#' \emph{Drug Inf J}. 1996; \strong{30}: 1105-1112.\cr
#' \doi{10.1177/009286159603000427}
#'
#' @source
#' See reference: Example data set shown in Table 1.
#'
#' @examples
#' str(dip1)
"dip1"


#' Dissolution data of one reference batch and five test batches
#'
#' A data set containing the dissolution data of one reference batch and five
#'   test batches of \eqn{n = 12} tablets each, i.e. the dissolution profiles
#'   of the \% drug release observed within a period of 180 minutes.
#'
#' @docType data
#'
#' @usage data(dip2)
#'
#' @format A data frame with 72 observations and 8 variables:
#' \describe{
#'   \item{type}{Factor with levels \code{Reference} and \code{Test}}
#'   \item{tablet}{Factor with levels \code{1} to \code{12} representing
#'     individual tablets}
#'   \item{batch}{Factor with levels \code{b0}, \code{b1}, \code{b2}, \code{b3},
#'     \code{b4} and \code{b5}}
#'   \item{t.0}{Numeric of the \% release at the initial testing point}
#'   \item{t.30}{Numeric of the \% release at the 30 minutes testing point}
#'   \item{t.60}{Numeric of the \% release at the 60 minutes testing point}
#'   \item{t.90}{Numeric of the \% release at the 90 minutes testing point}
#'   \item{t.180}{Numeric of the \% release at the 180 minutes testing point}
#' }
#'
#' @references
#' Shah, V. P., Tsong, Y., Sathe, P., and Liu, J. P. \emph{In vitro} dissolution
#' profile comparison - statistics and analysis of the similarity factor,
#' \eqn{f_2}. \emph{Pharm Res}. 1998; \strong{15}(6): 889-896.\cr
#' \doi{10.1023/A:1011976615750}
#'
#' @source
#' See reference: Example data set shown in Table 4.
#'
#' @examples
#' str(dip2)
"dip2"


#' Dissolution data of two different capsule formulations
#'
#' A data set containing the dissolution data of one reference batch and one
#'   test batch of \eqn{n = 12} capsules each, i.e. the dissolution profiles
#'   of the \% drug release observed at 15, 20 and 25 minutes.
#'
#' @docType data
#'
#' @usage data(dip3)
#'
#' @format A data frame with 24 observations and 6 variables:
#' \describe{
#'   \item{cap}{Factor with levels \code{1} to \code{12} representing individual
#'     capsules}
#'   \item{batch}{Factor with levels \code{white} and \code{blue} representing
#'     the colours of two different capsule formulations}
#'   \item{type}{Factor with levels \code{ref} (Reference) and \code{test}
#'     (Test)}
#'   \item{x.15}{Numeric of the \% release at the 15 minutes testing point}
#'   \item{x.20}{Numeric of the \% release at the 20 minutes testing point}
#'   \item{x.25}{Numeric of the \% release at the 25 minutes testing point}
#' }
#'
#' @references
#' Hoffelder, T., Goessl, R., and Wellek, S. Multivariate equivalence tests for
#' use in pharmaceutical development. \emph{J Biopharm Stat} (2015)
#' \strong{25}(3): 417-437.\cr
#' \doi{10.1080/10543406.2014.920344}
#'
#' @source
#' See reference: Example data set shown in Table 1. Data set
#' \sQuote{\code{ex_data_JoBS}} from package \sQuote{\code{T2EQ}}.
#'
#' @examples
#' str(dip3)
#'
#' if (requireNamespace("T2EQ")) {
#' library(T2EQ)
#'
#'   data(ex_data_JoBS, envir = environment())
#'   str(ex_data_JoBS)
#'   rm(ex_data_JoBS)
#' }
"dip3"


#' Dissolution data of two different formulations
#'
#' A data set containing the dissolution data of one reference batch and one
#'   test batch of \eqn{n = 12} items each, i.e. the dissolution profiles of
#'   the \% drug release observed at 10, 20 and 30 minutes.
#'
#' @docType data
#'
#' @usage data(dip4)
#'
#' @format A data frame with 24 observations and 2 variables:
#' \describe{
#'   \item{type}{Factor with levels \code{ref} (Reference) and \code{test}
#'     (Test)}
#'   \item{x.10}{Numeric of the \% release at the 10 minutes testing point}
#'   \item{x.20}{Numeric of the \% release at the 20 minutes testing point}
#'   \item{x.30}{Numeric of the \% release at the 30 minutes testing point}
#' }
#'
#' @references
#' Hoffelder, T. Highly variable dissolution profiles. Comparison of
#' \eqn{T^2}-test for equivalence and \eqn{f_2} based methods. \emph{Pharm Ind}.
#' 2016; \strong{78}(4): 587-592.\cr
#' \url{https://www.ecv.de/suse_item.php?suseId=Z|pi|8430}
#'
#' @source
#' See reference: Example data set underlying Figure 1. Data set
#' \sQuote{\code{ex_data_pharmind}} from package \sQuote{\code{T2EQ}}.
#'
#' @examples
#' str(dip4)
#'
#' if (requireNamespace("T2EQ")) {
#' library(T2EQ)
#'
#'   data(ex_data_pharmind, envir = environment())
#'   str(ex_data_pharmind)
#'   rm(ex_data_pharmind)
#' }
"dip4"


#' Fluid weights of drink cans
#'
#' The \code{response} values of this data set correspond to the values
#'   published in the SAS/QC(R) 13.1 (2013) User's Guide, Chapter 5 (The
#'   CAPABILITY Procedure). The data set is described on page 199: The fluid
#'   weights of 100 drink cans were measured in ounces. The filling process is
#'   assumed to be in statistical control.
#'
#' @docType data
#'
#' @usage data(dip5)
#'
#' @format A data frame with 100 observations and 3 variables:
#'   \describe{
#'   \item{type}{Factor with the single level \code{reference}}
#'   \item{batch}{Factor with levels \code{b1} to \code{b100}}
#'   \item{weight}{Weight of drink cans}
#' }
#'
#' @references
#' SAS Institute Inc. 2013. \emph{SAS/QC(R) 13.1 User's Guide}. Cary, NC:
#' SAS Institute Inc.\cr
#' \url{https://support.sas.com/documentation/cdl/en/qcug/66857/PDF/
#' default/qcug.pdf}
#'
#' @source
#' See reference: Chapter 5 (The CAPABILITY Procedure), Cans data set shown
#' on page 199.
#'
#' @examples
#' str(dip5)
"dip5"
