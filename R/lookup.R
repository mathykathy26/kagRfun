#' Lookup Function
#'
#'  Function to return a longer version of a variable name. The point is to be able to use short names of variables in a data frame, but be able to refer to a longer name such as for a plot label or elsewhere.
#' @param colname A character string that generally corresponds to a column names of a dataframe. This would generally be a short name, for which a longer name is desired for a plot or table.
#' @param longnames A vector of longer variable names, where the names of longnames should include colname.
#' @keywords search lookup
#' @export
#' @examples
#' test.dat<-data.frame(x=rnorm(10),y=rbinom(10,2,.5))
#' longnames <- c("verbal IQ","prenatal mercury exposure (ppm)")
#' names(longnames)<-dimnames(test.dat)[[2]]
#' lookup("y",longnames)


# file lookup.R
### lookup assumes that the user has created a vector of longer
### variable names, where the name of the vector matches the variable
### names in a dataset, e.g.
###      dimnames(test.dat)[[2]] <- c("y","x")
###      longnames <- c("verbal IQ","prenatal mercury exposure (ppm)")
###      names(longnames) <- names(test.dat)
### Then lookup, with proper arguments, will return the longer name, e.g.
###     yaxis <- lookup("y",longnames)
### Now yaxis will be "verbal IQ" instead of "y"
### The advantage here is that one can work with short names (saves typing)
### then use the longer names when needed for nice output.
###

lookup <- function(colname,longnames) {
  ### Function to return a longer version of a variable name.
  ### The point is to be able to use short names of variables
  ### in a data frame, but be able to refer to a longer name
  ### such as for a plot label or elsewhere.

  ### REQUIRED Arguments:
  ###      colname    = a character string that generally corresponds
  ###                   to a column names of a dataframe.  This
  ###                   would generally be a short name, for which
  ###                   a longer name is desired for a plot or table
  ###
  ###      longnames = a vector of longer variable names, where
  ###                   the names of longnames should
  ###                   include colname

  ### Return the "long" name from longnames, or if there
  ### is not a match, return the original variable name

  ### Example of some code prior to calling lookup:
  ###      dimnames(test.dat)[[2]] <- c("y","x")
  ###      longnames <- c("verbal IQ","prenatal mercury exposure (ppm)")
  ###      names(longnames) <- names(test.dat)
  ###      print(as.matrix(longnames)) # if want to verify naming
  ### Example of using lookup:
  ###     yaxis <- lookup("y",longnames)
  ### Now yaxis will be "verbal IQ" instead of "y"

  if (colname %in% names(longnames) == FALSE) {
    return(colname)
  } else {
    return(longnames[match(colname,names(longnames))])
  }
}


