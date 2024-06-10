#' Martinez & Zhao Tolerance Interval Approach
#'
#' The \emph{Martinez & Zhao Tolerance Interval Approach} (\code{mztia}) is a
#' simple approach for the comparison of dissolution profiles. The
#' \code{mztia()} function calculates tolerance intervals (\eqn{\textit{TI}})
#' at each time point of the dissolution profiles of a set of reference
#' batches. By aid of a graphical display the test batches are checked to lie
#' within the \eqn{\textit{TI}} boundaries or within certain limits exceeding
#' the \eqn{\textit{TI}} boundaries by a specified percentage.
#'
#' @param data A data frame with the dissolution profile data in wide or in
#'   long format (see parameter \code{shape}). If the data frame is in wide
#'   format, it is tried to extract the information on the time points of
#'   dissolution testing from the column names of the columns specified by
#'   the \code{tcol} parameter. Thus, they must contain extractable numeric
#'   information, e.g., \code{(t_0, t_5, t_10)}. If the data frame is in long
#'   format, it must have a column of time points (column specified via the
#'   \code{tcol} parameter).
#' @param shape A character string that indicates whether the data frame is
#'   in long or in wide format.
#' @param tcol If \code{shape} is \code{"wide"} an integer vector of indices,
#'   if \code{shape} is \code{"long"} an integer that specifies the column(s)
#'   containing the profile time points. If the data frame is in \code{wide}
#'   format it is reshaped using the function \code{\link[stats]{reshape}()}
#'   from the \sQuote{\code{stats}} package.
#' @param reference A character string that specifies the name of the reference
#'   group from the \code{grouping} variable.
#' @param response A character string that is expected if \code{data} is
#'   provided in long format to specify the column with the \% drug release
#'   values. The default is \code{NULL}.
#' @param alpha A numeric value between 0 and 1 that specifies the probability
#'   level. The default is \code{0.05}.
#' @param pp A numeric value between 0 and 1 that specifies the proportion of
#'   the population being enclosed by the tolerance interval boundaries. The
#'   default is \code{0.99}.
#' @param cap A logical variable that indicates whether the calculated
#'   tolerance limits should be limited (i.e. \emph{cap}ped). The default is
#'   \code{TRUE}.
#' @param bounds A numeric vector of the form \code{c(lower, upper)} that
#'   specifies the \dQuote{lower} and \dQuote{upper} limits, respectively, for
#'   the \% drug release at which the calculated tolerance interval limits
#'   should be capped (see parameter \eqn{cap}. This parameter is only relevant
#'   if \code{cap = TRUE}. The default is \code{c(0, 100)}.
#' @param qs A numeric vector of the form \code{c(Q S1, Q S2)} that specifies
#'   the allowable deviations from the specifications in percent according to
#'   the \eqn{S1} and \eqn{S2} acceptance criteria of USP chapter <711> on
#'   dissolution. The default is \code{c(5, 15)}.
#' @param ... Further arguments passed on to the \code{\link[stats]{reshape}()}
#'   from the \sQuote{\code{stats}} package.
#' @inheritParams get_T2_one
#' @inheritParams mimcr
#'
#' @details The tolerance interval approach proposed by Martinez & Zhao (2018)
#' is a simple approach for the comparison of dissolution profiles. The authors
#' propose to calculate for each time point of a set of reference dissolution
#' profiles a tolerance interval (\eqn{\textit{TI}}), i.e. intervals containing
#' \eqn{pp}\% of the population of potential values for reference product at a
#' probability level of \eqn{alpha / 2} per tail (i.e., \eqn{(1 - alpha) 100}\%
#' confidence). Based on these \eqn{\textit{TI}}s the dissolution profiles of
#' the test batch(es) is (are) compared, i.e. the corresponding data points
#' should lie within the \eqn{\textit{TI}}s. The \eqn{\textit{TI}}s are
#' calculated as
#'
#' \deqn{Y_{utl,ltl} = \bar{Y} \pm k \times s}{Y_{utl,ltl} = Y.bar +- k*s}
#'
#' where \eqn{\bar{Y}}{Y.bar} is the average, \eqn{s} is the sample standard
#' deviation, and the factor \eqn{k} is calculated according to Hahn (Hahn &
#' Meeker (1991)), as proposed in Martinez & Zhao (2018).
#'
#' Since the goal of the comparison is not to confirm simply
#' \dQuote{\emph{statistical sameness}} but \dQuote{product comparability},
#' Martinez & Zhao propose allowing acceptable deviations by utilizing the
#' concepts described by the United States Pharmacopoeia (USP), chapter <711>
#' on dissolution, defining \emph{allowable deviations from a set of product
#' specifications} (\eqn{Q}). The \eqn{\textit{TI}}s serve as the target value
#' \eqn{Q} at each sampling time. The allowable deviations about \eqn{Q} are
#' defined by the \eqn{S1} and \eqn{S2} acceptance criteria of USP chapter
#' <711> on dissolution:
#' \enumerate{
#' \item The \eqn{S1} level boundary is defined by \eqn{Q \pm 5}{Q +- 5}\%
#'   at each time point. For every 12 profiles tested, only one profile is
#'   allowed to exceed the \eqn{S1} bounds.
#' \item The \eqn{S2} level boundary is defined by \eqn{Q \pm 15}{Q +- 15}\%
#'   at each time point. No observation from any of the test dissolution
#'   profiles is allowed to exceed the \eqn{S2} bounds.
#' }
#'
#' In situations where the reference formulation itself has more than one of
#' twelve observations (profiles) exceeding \eqn{S1} at one or more time points,
#' additional runs of the reference product must be performed. It is deemed
#' appropriate to use the same values of \eqn{S1} and \eqn{S2} across all time
#' points because the high variability associated with the early sampling times
#' is already factored into the \eqn{\textit{TI}}s.
#'
#' \eqn{\textit{TI}} calculation according to Hahn is proposed because it
#' appeared to be more numerically stable and gave more consistent
#' \eqn{\textit{TI}}s than the \eqn{\textit{TI}} calculation method proposed
#' by Howe (Howe 1969) when samples were very variable. The reason might be due
#' to the less stringent requirements imposed by Hahn's method with respect to
#' the normality of the data.
#'
#' @return An object of class \sQuote{\code{mztia}} is returned, containing the
#' following elements:
#' \item{Variables}{A list of the variables and the corresponding values.}
#' \item{Limits}{A data frame of the limits calculated for each time point.}
#' \item{Data}{A data frame consisting of the provided data, complemented by
#'   the calculated tolerance interval results.}
#' \item{Profile.TP}{If \code{shape} is \code{"wide"} a named numeric vector
#'   of the columns in \code{data} specified by \code{tcol}. Given that the
#'   column names contain extractable numeric information, e.g., the testing
#'   time points of the dissolution profile, it contains the corresponding
#'   numeric values. Elements where no numeric information could be extracted
#'   are \code{NA}. If \code{shape} is \code{"long"} it is a numeric value that
#'   specifies the column containing the \% release values.}
#'
#' @references
#' Martinez, M.N., and Zhao, X. A simple approach for comparing the
#' \emph{in vitro} dissolution profiles of highly variable drug products: a
#' proposal. \emph{AAPS Journal}. 2018; \strong{20}: 78.\cr
#' \doi{10.1208/s12248-018-0238-1}
#'
#' Howe, W.G. Two-sided tolerance limits for normal populations - some
#' improvements. \emph{J Am Stat Assoc}. 1969; \strong{64}: 610-620.\cr
#' \doi{10.1080/01621459.1969.10500999}
#'
#' Hahn, G.J., and Meeker, W. Q. Statistical intervals: A guide for
#' practitioners. (1991); John Wiley & Sons, New York.
#' Hahn's method is also described in: SAS/QC 13.1: User's Guide. Chapter 5,
#' sub-chapter \dQuote{Details: INTERVALS Statement}, pp 421-424. SAS Institute
#' Inc. 2013. Cary, NC.\cr
#' \url{https://support.sas.com/documentation/cdl/en/qcug/66857/PDF/
#' default/qcug.pdf}
#'
#' U.S. Pharmacopoeia. 2016 U.S. Pharmacopoeia-National Formulary
#' (USP 39 NF 34). Volume 1. Rockville, Md: United States Pharmacopeial
#' Convention, Inc; 2015. <711> Dissolution.
#'
#' @seealso \code{\link{bootstrap_f2}}, \code{\link{mimcr}}.
#'
#' @example man/examples/examples_mztia.R
#'
#' @importFrom stats sd
#' @importFrom stats qnorm
#' @importFrom stats qchisq
#' @importFrom stats reshape
#' @importFrom stats aggregate
#' @importFrom stats na.omit
#'
#' @export

mztia <- function(data, shape, tcol, grouping, reference, response = NULL,
                  na_rm = FALSE, alpha = 0.05, pp = 0.99, cap = TRUE,
                  bounds = c(0, 100), qs = c(5, 15), ...) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.character(shape)) {
    stop("The parameter shape must be a string.")
  }
  if (!(shape %in% c("long", "wide"))) {
    stop("Please specify shape either as \"long\" or \"wide\".")
  }
  if (!is.numeric(tcol)) {
    stop("The parameter tcol must be an integer (vector).")
  }
  if (!isTRUE(all.equal(tcol, as.integer(tcol)))) {
    stop("The parameter tcol must be an integer (vector).")
  }
  if (min(tcol) < 1 || max(tcol) > ncol(data)) {
    stop("Some columns specified by tcol were not found in data frame.")
  }
  if (shape == "wide") {
    if (length(tcol) == 1) {
      stop("The parameter tcol has length 1. Did you provide a data frame in\n",
           "  long format? Then, the shape parameter should be \"long\" \n",
           "  instead of \"wide\". Alternatively, tcol should be changed\n",
           "  (specifying the profiles).")
    }
    if (sum(grepl("\\d", colnames(data[, tcol]))) < length(tcol)) {
      stop("Some names of columns specified by tcol ",
           "do not contain numeric information.")
    }
    if (sum(vapply(data[, tcol], is.numeric, logical(1))) != length(tcol)) {
      stop("Some columns specified by tcol are not numeric.")
    }
  }
  if (!is.character(grouping)) {
    stop("The parameter grouping must be a string.")
  }
  if (!(grouping %in% colnames(data))) {
    stop("The grouping variable was not found in the provided data frame.")
  }
  if (!is.factor(data[, grouping])) {
    stop("The grouping variable's column in data must be a factor.")
  }
  if (!is.character(reference)) {
    stop("The parameter reference must be a string.")
  }
  if (sum(levels(data[, grouping]) %in% reference) == 0) {
    stop("The reference variable was not found in the grouping column.")
  }
  if (shape == "long" && is.null(response)) {
    stop("When the data frame provided via data is in long format the ",
         "response parameter must be specified.")
  }
  if (!is.null(response)) {
    if (!is.character(response) || length(response) != 1) {
      stop("The parameter response must be a string of length 1.")
    }
    if (!(response %in% colnames(data))) {
      stop("The response variable was not found in the provided data frame.")
    }
    if (!is.numeric(data[, response])) {
      stop("The column specified by response is not numeric.")
    }
  }
  if (!is.logical(na_rm) || length(na_rm) > 1) {
    stop("The parameter na_rm must be a logical of length 1.")
  }
  if (alpha <= 0 || alpha > 1) {
    stop("Please specify alpha as (0, 1]")
  }
  if (pp <= 0 || pp > 1) {
    stop("Please specify pp as (0, 1]")
  }
  if (!is.logical(cap)) {
    stop("The parameter cap must be a logical.")
  }
  if (!is.numeric(bounds) || length(bounds) != 2) {
    stop("The parameter bounds must be a numeric vector of length 2.")
  }
  if (bounds[1] > bounds[2]) {
    stop("Please specify bounds in the form c(lower limit, upper limit).")
  }
  if (!is.numeric(qs) || length(qs) != 2) {
    stop("The parameter qs must be a numeric vector of length 2.")
  }
  if (sum(qs < 0) > 0 || sum(qs > 100) > 0) {
    stop("Please specify qs in the range [0, 100].")
  }
  if (qs[1] > qs[2]) {
    stop("Q S1 must be smaller Q S2.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Data preparation

  # Remove NA/NaN observations if desired
  if (na_rm == TRUE) {
    data <- na.omit(data)
  } else {
    if (any(is.na(data))) {
      message("Note that data contains NA/NaN values.\n",
              "  Please consider using the option na_rm = TRUE or\n",
              "  imputing missing values.")
    }
  }

  # Remove unused levels
  data <- droplevels(data)

  # Setting the response variable name
  response_vbl <- ifelse(!is.null(response), response, "response")

  # Generation of logical vector representing the reference and test group
  b1 <- data[, grouping] == reference

  # <-><-><-><->
  # Extraction of information on the time points

  # If the data are provided in wide format convert them into long format.
  if (shape == "wide") {
    time_points <- get_time_points(svec = colnames(data)[tcol])

    reshdat <- reshape(
      data = data,
      varying = colnames(data)[tcol],
      times = time_points, v.names = response_vbl, direction = "long", ...)
    reshdat$time.f <- as.factor(reshdat$time)
  } else {
    time_points <- tcol

    reshdat <- data
    reshdat$time <- as.numeric(b1)
    reshdat$time.f <- as.factor(reshdat$time)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculation, for the indicated reference, of the tolerance intervals at
  # each time point defined by the 'tcol' variable

  subdat <- droplevels(reshdat[reshdat[, grouping] == reference, ])

  n <- aggregate(subdat[, response_vbl], by = list(subdat$time.f),
                 FUN = function(x) length(x))$x
  df <- n - 1
  chisq_alpha <- qchisq(alpha, df)
  z_pp <- qnorm((1 + pp) / 2)
  kk <- (1 + 1 / (2 * n)) * z_pp * sqrt(df / chisq_alpha)

  t_mean <- aggregate(subdat[, response_vbl], by = list(subdat$time.f),
                      FUN = mean, na.rm = na_rm)$x
  t_sd <- aggregate(subdat[, response_vbl], by = list(subdat$time.f),
                    FUN = sd, na.rm = na_rm)$x
  t_x <- unique(subdat$time)

  ltl <- t_mean - kk * t_sd
  utl <- t_mean + kk * t_sd

  # Adjustment of the tolerance limit (if demanded)
  if (cap == TRUE) {
    ltl[ltl < bounds[1]] <- bounds[1]
    utl[utl > bounds[2]] <- bounds[2]
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Reporting of results

  # <-><-><-><->
  # Data frame of the results

  tmp1 <- data.frame(frame = rep("limits", 7 * length(t_x)),
                     grouping = rep(reference, 7 * length(t_x)),
                     type1 = c(rep("Mean", length(t_x)),
                               rep("TL", 2 * length(t_x)),
                               rep("TL.S1", 2 * length(t_x)),
                               rep("TL.S2", 2 * length(t_x))),
                     type2 = c(rep("Mean", length(t_x)),
                               rep("LTL", length(t_x)),
                               rep("UTL", length(t_x)),
                               rep("LTL", length(t_x)),
                               rep("UTL", length(t_x)),
                               rep("LTL", length(t_x)),
                               rep("UTL", length(t_x))),
                     time = rep(t_x, times = 7),
                     response = c(t_mean, ltl, utl,
                                  ltl - qs[1], utl + qs[1],
                                  ltl - qs[2], utl + qs[2]))

  if (!is.null(response)) {
    names(tmp1)[names(tmp1) == "response"] <- response_vbl
  }

  tmp2 <- data.frame(frame = rep("points", nrow(reshdat)),
                     grouping = reshdat[, grouping],
                     type1 = paste("Obs", reshdat[, grouping]),
                     type2 = paste("Obs", reshdat[, grouping]),
                     reshdat[, c("time", response_vbl)])

  d_res <- rbind(tmp2, tmp1)
  d_res$type1 <- factor(d_res$type1, levels =
                         c(paste("Obs", levels(reshdat[, grouping])),
                           "Mean", "TL", "TL.S1", "TL.S2"))
  d_res$type2 <- factor(d_res$type2, levels =
                          c(paste("Obs", levels(reshdat[, grouping])),
                            "Mean", "LTL", "UTL"))

  # <-><-><-><->
  # Data frame summarising the calculated limits

  tmp1$type3 <- interaction(tmp1$type1, tmp1$type2)
  tmp3 <- reshape(data = tmp1[tmp1$frame == "limits", ],
                  timevar = "type3",
                  idvar = "time",
                  drop = c("frame", "grouping", "type1", "type2"),
                  direction = "wide")
  colnames(tmp3) <-
    gsub("time", "Time",
         gsub(paste0(response_vbl, ".TL."), "",
              gsub(paste0(response_vbl, ".Mean.Mean"), "Mean", colnames(tmp3))))

  # <-><-><-><->
  # List of the variables

  l_variables <- list(shape = shape,
                      tcol = tcol,
                      grouping = grouping,
                      reference = reference,
                      response = response_vbl,
                      alpha = alpha,
                      pp = pp,
                      cap = cap,
                      bounds = bounds,
                      qs = qs)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compilation of results

  structure(list(Variables = l_variables,
                 Limits = tmp3,
                 Data = d_res,
                 Profile.TP = time_points),
            class = "mztia")
}

#' Graphical representation of the of MZTIA estimation
#'
#' The function \code{plot_mztia()} makes a graphical representation of the
#' estimates done by the \code{mztia()} function.
#'
#' @param x An object of class \sQuote{\code{mztia}} returned by the
#'   \code{\link{mztia}()} function.
#' @param ... Additional parameters that can be passed on to the
#'   \code{\link[ggplot2]{ggplot}()} function.
#'
#' @details A graphical representation of the information in the \code{Data}
#' element of the object that is returned by \code{mztia()} function is made
#' by aid of the \code{\link[ggplot2]{ggplot}()} function from the
#' \sQuote{\code{ggplot2}} package and added as new list element to the
#' \code{mztia} object. Ideally, the data frame provided to the
#' \code{\link{mztia}()} function allows drawing a time course of the \% drug
#' release values. If a single time point is available, the tolerance intervals
#' of the groups specified by the \code{grouping} parameter (e.g., for the
#' differentiation of batches or formulations of a drug product) are displayed.
#'
#' @return An object of class \sQuote{\code{plot_mztia}} is returned invisibly,
#' consisting of the elements of the \sQuote{\code{mztia}} object and an
#' additional element named \code{Graph}. The element \code{Graph} is a
#' \sQuote{\code{ggplot}} object returned by calling the
#' \code{\link[ggplot2]{ggplot}()} function.
#'
#' @seealso \code{\link{mztia}}, \code{\link[ggplot2]{ggplot}}.
#'
#' @example man/examples/examples_plot_mztia.R
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 geom_jitter
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 position_jitter
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 unit
#'
#' @export

plot_mztia <- function(x, ...) {
  if (!inherits(x, "mztia")) {
    stop("The parameter x must be an object of class mztia.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extraction of variables from the "Variable" element

  # Data
  model <- x
  d_res <- model[["Data"]]

  # Variables
  reference <- model[["Variables"]]$reference
  x <- "time"
  y <- model[["Variables"]]$response
  type <- "type1"

  if (length(unique(d_res[, x])) == 1) {
    d_lim <- model[["Limits"]]

    x <- grouping <- "grouping"
    ltl <- "LTL"
    utl <- "UTL"
    s1_ltl <- "S1.LTL"
    s1_utl <- "S1.UTL"
    s2_ltl <- "S2.LTL"
    s2_utl <- "S2.UTL"
    spread <- 0.1 * nlevels(d_res$grouping)

    d_lim <- data.frame(rep(reference, nrow(d_lim)), d_lim)
    colnames(d_lim) <- c("grouping", "time", y, colnames(d_lim)[4:ncol(d_lim)])
    d_lim$grouping <- as.factor(d_lim$grouping)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of vectors needed for plotting

  # Reference group points: grey; test group points: red; lines: various colours
  t_colours <-
    c(c("red", "grey80")[as.numeric(levels(d_res$grouping) == reference) + 1],
      c("royalblue", "olivedrab3", "darkorange1", "red1"))

  # Reference group: filled circles; test group: crosses; lines: .
  t_symbols <-
    c(c(4, 16)[as.numeric(levels(d_res$grouping) == reference) + 1],
      rep(46, 4))

  # Breaks and their labels
  t_breaks <- c(paste("Obs", levels(d_res$grouping)),
                "Mean", "TL", "TL.S1", "TL.S2")
  t_labels <- c(paste("Obs", levels(d_res$grouping)),
                "Mean", "TL", "TL \u00B1 S1 (5%)", "TL  \u00B1 S2 (15%)")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Generation of ggplot object

  if (x == "time") {
    ggraph <-
      ggplot(d_res,
             aes(x = .data[[x]], y = .data[[y]],
                 colour = .data[[type]], shape = .data[[type]]),
             ...) +
      geom_point(
        data = d_res[d_res$frame == "points" & d_res$grouping == reference, ],
        size = 1.5) +
      geom_point(
        data = d_res[d_res$frame == "points" & d_res$grouping != reference, ],
        size = 2) +
      geom_line(data = d_res[d_res$type2 == "Mean", ], linewidth = 1) +
      geom_path(data = d_res[d_res$type2 == "LTL", ], linewidth = 1) +
      geom_path(data = d_res[d_res$type2 == "UTL", ], linewidth = 1) +
      scale_colour_manual(
        values = t_colours, breaks = t_breaks, labels = t_labels,
        guide = guide_legend(
          override.aes =
            list(linetype = c("blank", "blank", rep("solid", 4))))) +
      scale_shape_manual(
        values = t_symbols, breaks = t_breaks, labels = t_labels) +
      theme_bw() + theme(legend.justification = c(1, 0),
                         legend.position = "inside",
                         legend.position.inside = c(1, 0.01),
                         legend.key.size = unit(1.5, "lines"),
                         plot.margin = unit(c(0.2, 0.4, 0.2, 0.2), "lines"),
                         panel.grid.major = element_line(colour = "grey80"),
                         title = element_text(size = 11.5),
                         plot.title = element_text(hjust = 0.5, vjust = -1),
                         axis.title = element_text(size = 12),
                         axis.text = element_text(size = 12),
                         axis.ticks.length = unit(0.5, "lines"),
                         legend.text = element_text(size = 11),
                         legend.title = element_blank(),
                         legend.key = element_rect(colour = "white"),
                         legend.key.height = unit(0.9, "line"))
  } else {
    ggraph <-
      ggplot(d_res,
             aes(x = .data[[x]], y = .data[[y]],
                 colour = .data[[grouping]], shape = .data[[grouping]]),
             ...) +
      geom_jitter(data = d_res[d_res$frame == "points", ],
                  position = position_jitter(spread)) +
      geom_errorbar(data = d_lim,
                    mapping = aes(ymin = .data[[s2_ltl]],
                                  ymax = .data[[s2_utl]]),
                    colour = "red1", width = 2 * spread,
                    linewidth = 0.8) +
      geom_errorbar(data = d_lim,
                    mapping = aes(ymin = .data[[s1_ltl]],
                                  ymax = .data[[s1_utl]]),
                    colour = "darkorange1", width = 2 * spread,
                    linewidth = 1.0) +
      geom_errorbar(data = d_lim,
                    mapping = aes(ymin = .data[[ltl]],
                                  ymax = .data[[utl]]),
                    colour = "cornsilk4", width = 2 * spread,
                    linewidth = 1.2) +
      theme_bw() + theme(legend.position = "none",
                         plot.margin = unit(c(0.2, 0.4, 0.2, 0.2), "lines"),
                         panel.grid.major = element_line(colour = "grey80"),
                         title = element_text(size = 11.5),
                         plot.title = element_text(hjust = 0.5, vjust = -1),
                         axis.title = element_text(size = 12),
                         axis.text = element_text(size = 12),
                         axis.ticks.length = unit(0.5, "lines"))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return object

  invisible(structure(list(Variables = model$Variables,
                           Limits = model$Limits,
                           Data = model$Data,
                           Graph = ggraph),
                      class = "plot_mztia"))
}
