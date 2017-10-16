#' Wait
#'
#' This function pauses a function during interactive sessions.
#' @keywords wait pause
#' @export
#' @examples
#' wait()


wait <- function()
{
   cat("Hit return to continue\n")
    ans <- readline()
  ans
}
