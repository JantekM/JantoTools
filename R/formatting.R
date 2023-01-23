# TODO: a better name for publishing-formatting family name
# TODO: add a way to use default setting loaded from the environment


#' Changes p-value number to a significance symbol for decorative purposes
#'
#' Any NAs in the input are always transformed to "". Probabilities outside range <0,1> raise an error.
#'
#' @param x A numeric vector of input probabilities
#' @param max_asterisks Number of asterisks (*) for the most significant results, default is \strong{5}. Cannot be smaller than 2. Set to 3 for APA/NEJM standards.
#' @param bonferoni Number of tests in a family of tests when applying the Bonferroni correction for multiple comparisons problem. Is \strong{1} by default which is same as not applying any correction.
#' Use it only if the passed p-values are not corrected already.
## @param NA_instead_of_error Logical, by default (\strong{FALSE}) if the functions sees any probability outside <0,1> it raises an error. However this behaviour can be changed to return NAs instead.
#' @param include_dot Logical, default \strong{TRUE}, should the dot symbol be used for borderline significance that is between 5% and 10%?
#' @param ns_sym Character (default \strong{' '}), the symbol used for the non-significant results. Set to 'ns' APA/NEJM standards.
#'
#' @return A character vector with significance symbols.
#' @export
#' @family publishing-formatting
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

#' Replaces Polish diacritic letters with their plain Latin equivalents
#'
#' Comes practical when some functions doesn't work well with UTF-8 characters. Like for example roxygen documentation generator ;-)
#'
#' @param string A character vector to be transformed
#'
#' @return A character vector without Polish diacritic letters
#' @export
#'
#' @examples
#' remove_diacritics("Wis\u0142a i \u017C\u00F3\u0142\u0107") # -> "Wisla i zolc"
#' # Unfortunately Roxygen documentation can have non ASCII characters only as escape characters
#'
remove_diacritics = function(string){
  chartr(
    "\u0105\u0104\u0144\u0143\u0107\u0106\u0119\u0118\u017C\u017B\u017A\u0179\u0142\u0141\u015B\u015A\u00F3\u00D3",
    "aAnNcCeEzZzZlLsSoO",
    string
  )
}

#' A handy function for formatting p-values to nice percents for publication
#'
#' ...
#'
#' TODO: Allow vectorized input
#' TODO: Add a way to automatically format them in APA/NEJM formats
#' TODO: Add a way to skip a trailing 0 and add p/P in the beginning
#' TODO: Add option to sanitize input
#'
#' @param p A single number with p-values to be formatted.
#' @param min.sig,max.sig Numbers in (0, 100) defining when function should return a thing like '<0.01%' or '>99.99%' instead of number.
#' By default \strong{0.001} and \strong{99.99}.
#'
#' @param sig.digs A number of significant digits to show. By default \strong{3}.
#'
#' @return A character vector with p-values formatted in a nice way
#'
#' @export
#' @family publishing-formatting
#'
#' @examples
#' formatP(0.0026485635335) # -> "0.265%"
#' formatP(5e-33) # -> "<0.001%"
formatP = function(p, min.sig = 0.001, max.sig = 99.99, sig.digs = 3){
  if(min.sig < 0 | min.sig >= 100 | max.sig > 100 | max.sig < 0){
    stop("Error in formatP(...), min.sig and max.sig must be in <0, 100>")
  }
  if(min.sig > max.sig)
    stop("Error in formatP(...), min.sig must be smaller than max.sig")
  if(p*100< min.sig){
    return(paste0('<', as.character(min.sig), '%'))
  }
  if(p*100> max.sig){
    return(paste0('>', as.character(max.sig), '%'))
  }
  paste0(as.character(signif(p*100,sig.digs)), '%')
}

#' A handy function for formatting ORs (Odds Ratios) to publication-ready format
#'
#' Especially handy when some other function has problems with displaying Inf
#'
#' TODO: Allow vectorized input
#' TODO: Add a way to include Confidence Intervals
#'
#' @param OR A single number with OR to be formatted, can be +Inf.
#' @param inf.as A string, defining how +Inf should be displayed as, \strong{'Inf'} by default.
#' @param sig.digs A number of significant digits to show. By default \strong{3}.
#'
#' @return A character vector with OR formatted in a nice way
#'
#' @export
#' @family publishing-formatting
#'
#' @examples
#' formatOR(0.0026485635335) # -> "0.00265"
#' formatOR(Inf) # -> "Inf"
formatOR = function(OR, inf.as = 'Inf', sig.digs = 3){
  if(OR == Inf){
    return(inf.as)
  }

  as.character(OR %>% signif(sig.digs))
}
