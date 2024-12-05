#' Calculates Odds Ratio
#'
#' Calculates the odds ratio for 2X2 contingency table
#' @param a 2x2 contingency table or an array of length 4
#' @return Odds Ratio
#' @export
odds.ratio = function(a)
{
  a = matrix(c(a), nrow = 2)
  value = (a[1,1] * a[2,2])/(a[1,2] * a[2,1])
  if(is.na(value)){stop("0/0 encountered while calculating odds ratio")}

  return(value)
}
