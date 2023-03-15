#' seedrrr
#'
#' Set RNG seed within a workflow.
#'
#' @param x Piped workflow or dataset presumably being used with some RNG methods.
#' @param seed Random seed number to use. Defaults to everyone's favourite number.
#'
#' @return Function output with the random seed taken into account.
#' @export
#'
#' @examples
#' rnorm(1) |> seedrrr()
#'
seedrrr <- function(x,
                    seed = 8675309) {

  set.seed(seed)

  return(x)

}
