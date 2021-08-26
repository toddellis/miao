#' trend_stats
#'
#' Applies a family of robust trend statistics: The Mann-Kendall S statistic, the Sen-Theil slope estimator, and Kendall's tau.
#'
#' @param x dbl : numeric timeseries variable
#' @param conf_level dbl : confidence level for upper- and lower-bounds for the Sen-Theil slope, defaults to 0.95 (p = 0.05)
#' @param alternative chr : one of three hypotheses to apply to Mann-Kendall statistics for testing trend direction
#' @param continuity lgl : TRUE or FALSE on whether to apply continuity correction to Mann-Kendall statistics. Same correction is always applied to Sen-Theil slope estimation.
#' @param na.rm lgl : TRUE or FALSE on whether to remove missing values. If missing values exist and this is FALSE, function will stop running.
#'
#' @return .bsen dbl : Sen-Theil slope estimator
#' @return .bsen_lwr dbl : Lower bounds for Sen-Theil slope estimator
#' @return .bsen_upr dbl : Upper bounds for Sen-Theil slope estimator
#' @return .mks dbl : Mann-Kendall S statistic
#' @return .mks_var dbl : Mann-Kendall trend variability
#' @return .mkz dbl : Mann-Kendall Z statistic
#' @return .ktau : Kendall's tau
#' @return .ktau_d : Kendall's tau D statistic
#' @return .p dbl : p-value for all trend statistics. Dependent on `continuity = TRUE`
#' @return .bsen_p dbl : p-value for Sen-Theil slope estimator and Kendall's tau(?). Dependent on `continuity = FALSE`
#' @return .mkz_p dbl : p-value for Mann-Kendall statistics. Dependent on `continuity = FALSE`
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(tidyverse)
#' dummy <- tibble::tibble(YEAR = c(2000:2020)) %>%
#'   rowwise() %>%
#'   mutate(RANDOM = runif(1, -33, 100)) %>%
#'   ungroup() %>%
#'   mutate(GRP = 'GROUP A') %>%
#'   bind_rows(tibble::tibble(YEAR = c(2005:2020)) %>%
#'               rowwise() %>%
#'               mutate(RANDOM = runif(1, 33, 100)) %>%
#'               ungroup() %>%
#'               mutate(GRP = 'GROUP B')) %>%
#'   bind_rows(tibble::tibble(YEAR = c(2000:2020)) %>%
#'               rowwise() %>%
#'               mutate(RANDOM = runif(1, 0, 1)) %>%
#'               ungroup() %>%
#'               mutate(GRP = 'GROUP C'))
#'  dummy %>%
#'    group_by(GRP) %>%
#'    summarise(trend_stats(RANDOM))
#'
#' }
#'

trend_stats <- function (x,
                        conf_level = 0.95,
                        alternative = c("two.sided",
                                        "greater",
                                        "less"),
                        continuity = TRUE,
                        na.rm = TRUE) {

  ## CALCULATE LENGTH OF TIMESERIES DATA ####
  .n <- length(x)

  ## TESTS TO ENSURE DATA INPUT IS CORRECT ####
  ## Is input numeric?
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector")
  }

  ## Is input longer than 3 rows?
  ## N.B. Originally only applicable to Mann-Kendall statistics, but why would we use this for a short timeseries, anyway!?
  if (.n < 3) {
    stop("'x' must have at least 3 elements")
  }

  ## BUILT-IN FUNCTIONS ####
  ## Calculate trend variability
  .varmk <- function(t, .n){
    tadjs <- sum(t * (t - 1) * (2 * t + 5))
    .mks_var <- (.n * (.n-1) * (2 * .n + 5) - tadjs) / 18
    return(.mks_var)
  }

  ## Calculate Mann-Kendall S statistics
  .mkScore <- function(x){
    .n <- length(x)
    .mks <- 0.0
    for(j in 1:.n) {
      .mks <- .mks + sum(sign(x[j] - x[1:j]))
    }
    return(.mks)
  }

  ## Calculate Kendall's tau D statistic
  .Dfn <- function (t, .n){
    tadjd <- sum(t * (t - 1))
    .ktau_d <- sqrt(1/2 * .n * (.n - 1) - 1/2 * tadjd) * sqrt(1/2 * .n *
                                                        (.n - 1))
    return(.ktau_d)
  }

  alternative <- match.arg(alternative)

  ## DEAL WITH MISSING VALUES ####
  ## TODO: Consider expanding on NA handling methods via zoo::na.approx or dplyr::replace_na ####
  if (na.rm) {
    na.omit(x)
  } else {
    na.fail(x)
  }

  ## CALCULATE BASIC STATS FOR TREND CALCULATION ####
  ## table() unique trend values
  t <- table(x)
  ## Remove reference to actual unique values and just get value occurences
  ## N.B. Not sure this step actually does anything important...
  names(t) <- NULL
  ## Calculate trend variability for S statistics
  .mks_var <- .varmk(t, .n)
  ## Set up starting point for comparing all trend values
  k <- 0
  ## Create empty vector to fill with year-by-year value comparisons
  d <- rep(NA, .n * (.n - 1)/2)
  ## Fill empty matrix by comparing all years to one another
  for (i in 1:(.n - 1)) {
    for (j in (i + 1):.n) {
      k <- k + 1
      d[k] <- (x[j] - x[i])/(j - i)
    }
  }
  ## CALCULATE SEN-THEIL SLOPE ESTIMATOR ####
  .bsen <- median(d, na.rm = TRUE)
  ## Calculate confidence interval for Sen-Theil slope
  C <- qnorm(1 - (1 - conf_level)/2) * sqrt(.mks_var)
  rank.bsen_upr <- round((k + C)/2 + 1)
  rank.bsen_lwr <- round((k - C)/2)
  rank.d <- sort(d)
  .bsen_lwr <- rank.d[rank.bsen_lwr]
  .bsen_upr <- rank.d[rank.bsen_upr]
  ## CALCULATE MANN-KENDALL S STATISTIC ####
  .mks <- .mkScore(x)
  ## Calculate Kendall's tau D statistic
  .ktau_d <- .Dfn(t, .n)
  ## CALCULATE KENDALL'S TAU ####
  .ktau <- .mks/.ktau_d
  ## CALCULATE MANN-KENDALL Z STATISTICS ####
  ## Apply continuity correction if applicable
  ## N.B. Sen-Theil slope estimator p-values always assume `continuity = TRUE`
  if (continuity) {
    ## Calculate signum of S statistic
    sg <- sign(.mks)
    ## Calculate Z statistic to represent both Mann-Kendall S statistic and Sen-Theil slope estimator
    .mkz <- sg * (abs(.mks) - 1)/sqrt(.mks_var)
    ## Copy-paste Z statistic for Sen (just for p-value purposes)
    sen.z <- .mkz
    ## Or don't apply continuity correction and calculate two distinct Z statistics for Mann-Kendall S statistics and Sen-Theil slope estimators
  } else {
    ## calculate signum of Mann-Kendall S statistic
    ## N.B. Used only for Sen-Theil slope estimator p-value calculation, which assumes `continuity = TRUE`
    sg <- sign(.mks)
    ## Calculate Mann-Kendall Z statistic
    .mkz <- .mks/sqrt(.mks_var)
    ## Calculate Sen-Theil slope estimator Z statistic
    sen.z <- sg * (abs(.mks) - 1)/sqrt(.mks_var)
  }

  ## CALCULATE P-VALUES ####
  ## Calculate Sen-Theil slope estimator p-value
  ## N.B. Always assumes `continuity = TRUE`
  .bsen_p <- 2 * min(0.5, pnorm(abs(sen.z), lower.tail = FALSE))
  ## Calculate Mann-Kendall Z statistic p-value
  ## N.B. Dependent on `continuity` and the selected `alternative` hypothesis for trend direction.
  .mkz_p <- switch(alternative,
                   two.sided = 2 * min(0.5, pnorm(abs(.mkz), lower.tail = FALSE)),
                   greater = pnorm(.mkz, lower.tail = FALSE),
                   less = pnorm(.mkz, lower.tail = TRUE))

  ## If Sen-Theil and Mann-Kendall p-values are identical, combine them in the output.
  if (.bsen_p == .mkz_p) {
    df_output <- tibble(.n,
                        .bsen,
                        .bsen_lwr,
                        .bsen_upr,
                        .mks,
                        .mks_var,
                        .mkz,
                        .ktau,
                        .ktau_d,
                        .p = .bsen_p)
    ## Otherwise, spit out both methods p-values.
    ## N.B. Not sure which one represents Kendall's tau.
    ##      Kendall's tau is originally only calculated in trend::mk.test, but is not influenced by `continuity`. `mk.test` only spits out one p-value.
    ##      trend::sens.slope *always* takes `continuity = TRUE` into account when calculating the p-value for the Sen-Theil slope estimator.
  } else {
    df_output <- tibble(.n,
                        .bsen,
                        .bsen_lwr,
                        .bsen_upr,
                        .bsen_p,
                        .mks,
                        .mks_var,
                        .mkz,
                        .mkz_p,
                        .ktau,
                        .ktau_d)
  }
  ## RETURN TIBBLE OUTPUT ####
  return(df_output)
}

#' FUNCTION    : trend_stats
#' AUTHOR      : todd.ellis@utas.edu.au
#' DATE        : 2021-08-26 : 2021-07-25
#' DESCRIPTION : Dataframe-friendly adaptation of Sen-Theil, Mann-Kendall, and Kendall trend statistics.
#' NOTES       : Heavily drawn from the `trend` package, which only works on single vectors and returns a uniquely-defined list object. (Ew!)
#' TODO        : 1. Considering replacing na.rm with zoo::na.approx(x, na.rm = FALSE). Sometimes errors in testing, however. Not sure why.
#'             : 2. Is there a better way to implement this function rather than within dplyr::summarise(...)?
