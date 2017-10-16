#' Adaptive Package Installation and Loading
#'
#' This function allows you to load packages already installed and install those missing.
#' @param vec.of.packages Character vector of package names.
#' @keywords library install
#' @export
#' @examples
#' library.install(c("dplyr","knitr"))
#' library.install("knitr")

library.install<-function(vec.of.packages){
  new.packages <- vec.of.packages[!(vec.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)

  temp<-sapply(vec.of.packages,library,character.only=T,warn.conflicts=FALSE,quietly=T)
}
