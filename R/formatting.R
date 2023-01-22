#' Changes p-value number to a significance symbol for decorative purposes
#'
#' @param x A number vector with one element
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x <- 3e-4
#' signif_sym(x)
signif_sym <- function(x) {
  as.character(stats::symnum(
    x,
    corr = FALSE,
    na = FALSE,
    legend = FALSE,
    cutpoints = c(0, 0.00001, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("*****", "****", "***", "**", "*", ".", " ")
  ))
}
