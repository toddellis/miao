#' purrrallel
#'
#' Set parallel processing clusters within a workflow.
#'
#' @param x Piped workflow or dataset with some methods that could use parallel processing.
#' @param nclust Number of clusters to set.
#'
#' @return Function output with number of clusters applied and then flushed.
#' @export
#'
#'
purrrallel <- function(x,
                       nclust = NULL,
                       seed = 8675309) {

  set.seed(seed)

  if (is.null(nclust) | !is.numeric(nclust) | nclust > parallel::detectCores(logical = TRUE)) {
    .nclust <-
      (parallel::detectCores(logical = F) / 2)

    warning(glue::glue("No clusters defined. Defaulting to {.nclust} as defined by `parallel::detectCores(logical = FALSE) / 2`."))
  } else {

    .nclust <- nclust

  }

  .clusters <-
    parallel::makePSOCKcluster(.nclust)

  doParallel::registerDoParallel(.clusters)

  .output <-
    x

  parallel::stopCluster(.clusters)

  return(.output)

}
