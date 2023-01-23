#' Changes p-value number to a significance symbol for decorative purposes.
#'
#' Any NAs in the input are always transformed to "". Probabilities outside range <0,1> raise an error.
#'
#' @param x A numeric vector of input probabilities
#' @param max_asterisks Number of asterisks (*) for the most significant results, default is 5. Cannot be smaller than 2. Set to 3 for APA/NEJM standards.
#' @param bonferoni Number of tests in a family of tests when applying the Bonferroni correction for multiple comparisons problem. Is 1 by default which is same as not applying any correction.
#' Use it only if the passed p-values are not corrected already.
## @param NA_instead_of_error Logical, by default (FALSE) if the functions sees any probability outside <0,1> it raises an error. However this behaviour can be changed to return NAs instead.
#' @param include_dot Logical, default TRUE, should the dot symbol be used for borderline significance that is between 5% and 10%?
#' @param ns_sym Character (default ' '), the symbol used for the non-significant results. Set to 'ns' APA/NEJM standards.
#'
#' @return A character vector with significance symbols.
#' @export
#'
#' @examples
#' x <- 3e-4
#' signif_sym(x)
signif_sym <-
  function(x,
           max_asterisks = 5,
           bonferoni = 1,
           #NA_instead_of_error = F,
           include_dot = T,
           ns_sym = ' ') {

  #if(!NA_instead_of_error){
  if(any(stats::na.omit(x) < 0) | any(stats::na.omit(x)>1)){
    stop("Error in signif_sym(...), passed probabilities cannot be smaller than 0 or bigger than 1.")
  }
  #}
  if(max_asterisks<2){
    stop("Error in signif_sym(...), max_asterisks param has to be at least 2.")
  }
  if(is.na(ns_sym)){
    stop("Error in signif_sym(...), ns_sym cannot be set to NA.")
  }
  cutpoints = rep(0.1, max_asterisks-1) ^ seq(from = max_asterisks, to = 2)
  cutpoints = c(0, cutpoints, 0.05, ifelse(include_dot, 0.1, NA), 1) %>% stats::na.omit()
  symbols = sapply(max_asterisks:1, function(x){paste(rep('*', x), collapse='')})
  symbols = c(symbols,ifelse(include_dot, '.', NA), ns_sym) %>% stats::na.omit()

  # output for default 5 asterisks with dot and space as ns_sym
  # cutpoints = c(0,       0.00001, 0.0001, 0.001, 0.01, 0.05, 0.1, 1)
  # symbols =   c("*****", "****",  "***",  "**",  "*",  ".",  " ")

  as.character(stats::symnum(
    x/bonferoni,
    corr = FALSE,
    na = T,
    legend = FALSE,
    cutpoints = cutpoints,
    symbols = symbols
  ))
}



