#' Geometric Mean
#'
#' This function allows you to calculate the geometric mean of a vector of positive numbers.
#' @param x Vector of positive numbers.
#' @keywords geometric mean
#' @export
#' @examples
#' geomean(c(2.2,5.3,7,1,0.2))

geomean<-function(x){exp(mean(log(x),na.rm=T))}
