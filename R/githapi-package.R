#  PACKAGE: githapi ------------------------------------------------------------
#
#' User-friendly access to the GitHub API for R, consistent with the tidyverse
#'
#' Provides a suite of functions which simplify working with GitHub's API. The
#' functions have a consistent API and return either lists for single entities
#' and tibbles for collections.
#'
#' @name githapi
#' @docType package
#'
#' @seealso
#' Useful links:
#' - \url{https://github.com/ChadGoymer/githapi}
#' - Report bugs at \url{https://github.com/ChadGoymer/githapi/issues}
#'
#' @author Chad Goymer \email{chad.goymer@@gmail.com}
#'
#' @importFrom methods as is
#' @importFrom rlang is_integerish is_scalar_integerish
#' @import stringr purrr tibble dplyr msgr
#'
# Global variables used in dplyr pipelines
globalVariables(c(".", ".data"))

# Environment to cache tokens
.cache <- new.env()
