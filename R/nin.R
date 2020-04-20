#' @title \%nin\%
#'
#' @description Opposite of the \%in\% operator. Tests if X is NOT in Y.
#'
#' @usage x \%nin\% y
#'
#' @param x vector or \code{NULL}: the values to be matched
#'
#' @param y vector or \code{NULL}: the values to be matched against
#'
#' @author David Wilkinson \email{davidpw@student.unimelb.edu.au}
#'
#' @section Date submitted: 2019-03-19
#'
#' @section Last Modified: 2019-03-19
#'
#' @examples 20 %nin% 1:10
#'
#' @return A logical vector
#'
#' @export

## Credit: STATWORX
## https://github.com/STATWORX/helfRlein

`%nin%` <- Negate(`%in%`)


