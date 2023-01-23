
#' Infix not in operator
#'
#' Function being basically the opposite of %in% operator. Strictly a QoL thing
#'
#' @param lhs An object or vector
#' @param rhs A vector (a set)
#' @return The vector of logical values being the negated %in% result
#'
#' @export
#' @family QoL
#'
#' @examples
#' \dontrun{
#'  1 %notin% c(2, 1, 3) # -> F
#'  c(1, 5, 10) %notin% c(2,1,3) # -> F T T
#' }
'%notin%' <- function(lhs,rhs)
{
  !('%in%'(lhs,rhs))
}
