#' File Names Using Current Dates
#'
#'  Function returns a character string consisting of the user-supplied character string and the current date in %Y-%m$d format.
#' @param ... A character string intended to be the file name.
#' @keywords date
#' @export
#' @examples
#' paste.date("Todays Analysis.Rmd")

########################################
##
## Pastes the date in the format Kevin
##  likes to whatever else you enter
##
########################################

paste.date<-function(...) return(paste0(format(Sys.time(), "%Y-%m%d"),"-",...))
