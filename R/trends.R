#' trends
#'
#' Applies a family of trend statistics: The Mann-Kendall S statistic, the Sen-Theil slope estimator, and Kendall's tau.
#'
#' @param x Timeseries vector.
#' @param conf_level Confidence level used for calculating the upper and lower bounds of the Sen-Theil slope estimator. Defaults to 95%.
#' @param alt_hypo Alternative hypothesis applied in the Mann-Kendall S statistic.
#' @param continuity Logical whether to apply continuity correction to Mann-Kendall statistics. Same correction is always applied to Sen-Theil slope estimation.
#' @param na Method reference for handling missing values. Missing values are dropped by default, but can also be replaced using a linear approximation, the nearest value, or a summary statistic (mean, median, min, max).
#'
#' @return Dataframe of all relevant trend statistics from the Sen-Theil, Mann-Kendall, and Kendall families of statistics.
#' @export
#'
#' @examples
#' \dontrun{
#' dummy <- tibble::tibble(YEAR = c(2000:2020)) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(RANDOM = runif(1, -33, 100)) %>%
#'   dplyr::ungroup() %>%
#'   dplyr::mutate(GRP = 'GROUP A') %>%
#'   dplyr::bind_rows(tibble::tibble(YEAR = c(2005:2020)) %>%
#'                    dplyr::rowwise() %>%
#'                    dplyr::mutate(RANDOM = runif(1, 33, 100)) %>%
#'                    dplyr::ungroup() %>%
#'                    dplyr::mutate(GRP = 'GROUP B')) %>%
#'   dplyr::bind_rows(tibble::tibble(YEAR = c(2000:2020)) %>%
#'                    dplyr::rowwise() %>%
#'                    dplyr::mutate(RANDOM = runif(1, 0, 1)) %>%
#'                    dplyr::ungroup() %>%
#'                    dplyr::mutate(GRP = 'GROUP C'))
#'  dummy %>%
#'    dplyr::group_by(GRP) %>%
#'    dplyr::mutate(RANDOM = ifelse(RANDOM >= 80,
#'                                  NA,
#'                                  RANDOM)) %>%
#'    dplyr::summarise(trends(RANDOM,
#'                            conf_level = 0.95,
#'                            alt_hypo = 'two-tailed',
#'                            continuity = TRUE,
#'                            na = 'median'))
#'
#' }
#'

trends <-  function(x,
                    conf_level = 0.95,
                    alt_hypo = c("two-tailed",
                                 "greater",
                                 "less"),
                    continuity = TRUE,
                    na = c('rm', 'drop',
                           'approx', 'lm',
                           'fill', 'extend',
                           'aggregate', 'mean',
                           'median',
                           'min',
                           'max',
                           'fail')) {

  ## TESTS TO ENSURE DATA INPUT IS CORRECT ####
  ## Is input numeric?
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.")
  }

  ## HELPER FUNCTIONS VIA `trend` PACKAGE ####
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

  .alt_hypo <- match.arg(alt_hypo)
  .na <- match.arg(na)

  ## DEAL WITH MISSING VALUES ####
  ## TODO: Consider expanding on NA handling methods via zoo::na.approx or dplyr::replace_na ####
  if (.na %in% c('drop', 'rm')) {
    x <- na.omit(x)
  } else if (.na %in% c('approx', 'lm')) {
    x <- tryCatch(zoo::na.approx(x,
                                 na.rm = FALSE),
                  error = function(cond) {
                    print(paste0('`zoo::na.approx` failed. Removing NAs instead.'))
                    return(na.omit(x))
                  })

    if (sum(is.na(x)) >= 1) {
      warning('NA replacement method did not replace all NAs. Remaining NAs are being dropped.')
    }

    x <- na.omit(x)

  } else if (.na %in% c('fill', 'extend')) {
    x <- tryCatch(zoo::na.fill(x,
                               'extend'),
                  error = function(cond) {
                    print(paste0('`zoo::na.fill` failed. Removing NAs instead.'))
                    return(na.omit(x))
                  })
  } else if (.na %in% c('aggregate', 'mean')) {
    x <- tryCatch(zoo::na.aggregate(x,
                                    FUN = mean,
                                    na.rm = FALSE),
                  error = function(cond) {
                    print(paste0('`zoo::na.aggregate` failed. Removing NAs instead.'))
                    return(na.omit(x))
                  })
  } else if (.na == 'median') {
    x <- tryCatch(zoo::na.aggregate(x,
                                    FUN = median,
                                    na.rm = FALSE),
                  error = function(cond) {
                    print(paste0('`zoo::na.aggregate` failed. Removing NAs instead.'))
                    return(na.omit(x))
                  })
  } else if (.na == 'max') {
    x <- tryCatch(zoo::na.aggregate(x,
                                    FUN = max,
                                    na.rm = FALSE),
                  error = function(cond) {
                    print(paste0('`zoo::na.aggregate` failed. Removing NAs instead.'))
                    return(na.omit(x))
                  })
  } else if (.na == 'min') {
    x <- tryCatch(zoo::na.aggregate(x,
                                    FUN = min,
                                    na.rm = FALSE),
                  error = function(cond) {
                    print(paste0('`zoo::na.aggregate` failed. Removing NAs instead.'))
                    return(na.omit(x))
                  })
  } else if (.na == 'fail') {
    na.fail(x)
  }

  ## CALCULATE LENGTH OF TIMESERIES DATA ####
  .n <- length(x)
  ## Is input longer than 3 rows?
  ## N.B. Originally only applicable to Mann-Kendall statistics, but why would we use this for a short timeseries, anyway!?
  if (.n < 3) {
    stop("'x' must have at least 3 elements")
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
    .mks_sg <- sign(.mks)
    ## Calculate Z statistic to represent both Mann-Kendall S statistic and Sen-Theil slope estimator
    .mks_z <- .mks_sg * (abs(.mks) - 1)/sqrt(.mks_var)
    ## Copy-paste Z statistic for Sen (just for p-value purposes)
    .bsen_z <- .mks_z
    ## Or don't apply continuity correction and calculate two distinct Z statistics for Mann-Kendall S statistics and Sen-Theil slope estimators
  } else {
    ## calculate signum of Mann-Kendall S statistic
    ## N.B. Used only for Sen-Theil slope estimator p-value calculation, which assumes `continuity = TRUE`
    .mks_sg <- sign(.mks)
    ## Calculate Mann-Kendall Z statistic
    .mks_z <- .mks/sqrt(.mks_var)
    ## Calculate Sen-Theil slope estimator Z statistic
    .bsen_z <- .mks_sg * (abs(.mks) - 1)/sqrt(.mks_var)
  }

  ## CALCULATE P-VALUES ####
  ## Calculate Sen-Theil slope estimator p-value
  ## N.B. Always assumes `continuity = TRUE`
  .bsen_p <- 2 * min(0.5, pnorm(abs(.bsen_z), lower.tail = FALSE))
  ## Calculate Mann-Kendall Z statistic p-value
  ## N.B. Dependent on `continuity` and the selected `alternative` hypothesis for trend direction.
  .mks_p <- switch(.alt_hypo,
                   'two-tailed' = 2 * min(0.5, pnorm(abs(.mks_z), lower.tail = FALSE)),
                   greater = pnorm(.mks_z, lower.tail = FALSE),
                   less = pnorm(.mks_z, lower.tail = TRUE))

  ## If Sen-Theil and Mann-Kendall p-values are identical, combine them in the output.
  if (.bsen_p == .mks_p & .bsen_z == .mks_z) {
    df_output <- tibble(.n,
                        .bsen,
                        .bsen_lwr,
                        .bsen_upr,
                        .mks,
                        .mks_sg,
                        .mks_var,
                        .ktau,
                        .ktau_d,
                        .z = .mks_z,
                        .p = .mks_p)
    ## Otherwise, spit out both methods p-values.
    ## N.B. Not sure which one represents Kendall's tau.
    ##      Kendall's tau is originally only calculated in trend::mk.test, but is not influenced by `continuity`. `mk.test` only spits out one p-value.
    ##      trend::sens.slope *always* takes `continuity = TRUE` into account when calculating the p-value for the Sen-Theil slope estimator.
  } else {
    df_output <- tibble(.n,
                        .bsen,
                        .bsen_lwr,
                        .bsen_upr,
                        .bsen_z,
                        .bsen_p,
                        .mks,
                        .mks_sg,
                        .mks_var,
                        .mks_z,
                        .mks_p,
                        .ktau,
                        .ktau_d)
  }
  ## RETURN TIBBLE OUTPUT ####
  return(df_output)
}

#' FUNCTION    : trend_stats
#' AUTHOR      : todd.ellis@utas.edu.au
#' DATE        : 2021-09-04 : 2021-01-15 (est.)
#' DESCRIPTION : Dataframe-friendly adaptation of Sen-Theil, Mann-Kendall, and Kendall trend statistics.
#' NOTES       : Heavily drawn from the `trend` package, which only works on single vectors and returns a uniquely-defined list object. (Ew!)
#' TODO        : Is there a better way to implement this function rather than within dplyr::summarise(...)?
