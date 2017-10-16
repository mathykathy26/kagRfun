#' Summary Table Creation
#'
#' This function allows you to create a summary of variables table with LaTeX formatting options.
#' @param tmat Data matrix
#' @param digits The number of digits to round the values to.
#' @param quants Vector of quantiles to include in the table. Values should be in [0,1].
#' @param SD Logical, include a column for standard deviations. Defaults to TRUE.
#' @param nmis Logical, include a column for the number of missing observations.
#' @param uniq Logica, include a column for the number of categories in the variable. Defaults to FALSE.
#' @param str Logical, include a column for the class of each variable. Defaults to FALSE.
#' @param latex Logical, returns table in LaTeX format via the xtable package.
#' @param full.latex Logical, TRUE if want latex output to be a stand-alone LaTeX file.  If latex=TRUE and full.latex=FALSE, the output can be included with an \include{filename} command. This may be useful if output from this function will be included as one part of a larger set of function calls.
#' @param lookup.names Vector of longer descriptive variable names, where column names should match variable names in tmat.
#' @param filename Name of file where output should be saved. Default is NULL, not to save it to a file.
#' @param screen Logical, TRUE if want to view results on the screen. Defaults to FALSE.
#' @param caption Caption for LaTeX format
#' @keywords summary table xtable
#' @export
#' @examples
#' test.dat<-data.frame(x=rnorm(10),y=as.factor(rbinom(10,1,.5)))
#' summarize.class(test.dat)

### file summarize.R:created in R version 2.8.0
###
### This function gives a 1-line summary for each
### variable in a dataframe.  The summary can be as short as just the
### name, n, mean, or can include SD, number missing (nmis), number of
### unique values or categories (uniq), and any quantiles that the
### user wants.  User can use lookup to list the variables by the
### longer name.  Output can be put to the screen, and/or output to a
### file.  The output can either be in tabular format, or in a LaTeX
### table.

summarize.class <- function(tmat,
                      digits=2,
                      quants=c(0,0.25,0.5,0.75,1),
                      SD=TRUE,
                      nmis=FALSE,
                      uniq=FALSE,
                      str=FALSE,
                      latex=FALSE,
                      full.latex=FALSE,
                      lookup.names=NULL,
                      filename=NULL,
                      screen=FALSE,
                      caption=NULL) {

  ### REQUIRED arguments are:
  ###      tmat = dataframe containing variable for which we want a summary
  ### OPTIONAL arguments are:
  ###     digits = number of digits to show in the summary
  ###     quants = quantiles to be included. Default is c(0,.25,.5,.75,1)
  ###             If don't want quantiles, make quants=NULL
  ###     latex = TRUE if want output in LaTeX
  ###     full.latex = TRUE if want latex output to be a stand-alone
  ###             LaTeX file.  If latex=TRUE and full.latex=FALSE, the
  ###             output can be included with an \include{filename} command.
  ###             (may be useful if output from this function will be
  ###             included as one part of a larger set of function calls.)

  require(xtable)


  ### if tmat is a vector, turn into matrix
  if (is.vector(tmat)) tmat <- as.matrix(tmat)

  ### if more than 1 variable, set matnames to be name of variables,
  ### and count number of variables
  if (ncol(tmat) > 1) {
    if(length(dimnames(tmat)[[2]])==0) stop("Need dimnames for a matrix\n")
    matnames <- dimnames(tmat)[[2]]
    nvars <- length(matnames)
  } else {
    nvars <- 1
  }

  if (!is.null(quants)) {
    if ((min(quants) < 0) | (max(quants) > 1)) stop("Quantiles must be in (0,1)")
  }

  ### Create a matrix, summaries, in which to store results.  First figure out # cols
  ### Always have cols for name, n, mean
  nopt1 <- 1*(SD==1) + 1*(nmis==1) + 1*(uniq==1)
  #  cat("nopt1=",nopt1,"\n")
  if (is.null(quants)) nopt2 <- 0 else nopt2 <- length(quants)
  #  cat("nopt2=",nopt2,"\n")
  number.cols <- 3 + nopt1 + nopt2
  #  cat("number.cols=",number.cols,"\n")
  summaries <- as.data.frame(matrix(nrow=nvars,ncol=number.cols))
  summaries.names <- "nobs"
  if (nmis) summaries.names <- c(summaries.names,"nmis")
  if (uniq) summaries.names <- c(summaries.names,"uniq")
  if (str)  summaries.names <- c(summaries.names,"class")
  summaries.names <- c(summaries.names,"mean")
  if (SD) summaries.names <- c(summaries.names,"SD")

  if (!is.null(quants)) {
    quantnames <- as.character(quants)
    #    print(quantnames)
    for (j in 1:length(quantnames)) {
      #      cat("j=",j,"quantnames[j]=",quantnames[j],"\n")
      quantnames[j] <- paste(as.character(quants[j]*100),"%",sep="")
      if (quants[j]==0) quantnames[j] <- "min"
      if (quants[j]==1) quantnames[j] <- "max"
    }

    summaries.names <- c(summaries.names,quantnames)
  }

  colnames(summaries) <- summaries.names

  ### MAIN PART OF PROGRAM: fill values of summaries
  ### Loop through each variable in the dataframe
  for (j in 1:ncol(tmat)) {
    ### If more than one variable, assign row names - use lookup
    ### to give a more descriptive name, if there is one
    if (ncol(tmat) > 1) {
      if (!is.null(lookup.names)) {
        rownames(summaries)[j] <- lookup(matnames[j],lookup.names)
      } else {
        rownames(summaries)[j] <- colnames(tmat)[j]
      }
    }
    #    if (ncol(tmat) > 1) rownames(summaries)[j] <- lookup(matnames[j],lookup.names)
    thisvar <- tmat[,j]
    ### define vector where 1's are observed values
    OK.nomiss <- !is.na(thisvar)
    ### Count number of non-missing and missing values
    summaries[j,"nobs"] <- sum(OK.nomiss)

    ### If want long summary, count number of missing values, and
    ### number of categories (excluding missing values),
    if (nmis)  summaries[j,"nmis"] <- sum(is.na(thisvar))
    if (uniq)  summaries[j,"uniq"] <- length(unique(thisvar[OK.nomiss]))
    if (str)   summaries[j,"class"]<- substring(class(thisvar),1,3)

    if(!(class(thisvar)%in%c("numeric","integer","double"))) thisvar<-as.numeric(thisvar)-1

    summaries[j,"mean"] <- round(mean(thisvar[OK.nomiss]),digits)
    if (SD)  summaries[j,"SD"] <- round(sd(thisvar[OK.nomiss]),digits)

    ### If the variable is numeric, get mean and quantiles
    if (is.numeric(thisvar)) {
      if (!is.null(quants)) summaries[j,quantnames] <- round(quantile(thisvar[OK.nomiss],probs=quants),digits)
    }
  }  ### End of looping through each variable


  ### Output results
  output.results <- function() {
    ### if user wants results as LaTeX, use xtable to make a LaTeX table
    if (latex) {
      ### if user wants to save as full LaTeX file, need commands at top
      if (full.latex) {
        cat("\\documentclass{article}")
        cat("\\begin{document}")
      }
      print(xtable(summaries,floating=FALSE,table.placement="H!",caption=caption,digits=digits))
      ### if save as full LaTeX file, need commands at bottom
      if (full.latex)  cat("\\end{document}\n")
      ### if user doesn't want LaTeX, just print the results
    }  else print(summaries)
  }  ### end of output.results


  ### If user wants to view results on screen
  if (screen) {
    output.results()
  }

  ### If user wants to save results to a file,
  ### open filename, call on output.results(), close file.
  if (!is.null(filename)) {
    if (latex) sink(paste(filename,".tex",sep=""))
    else sink(paste(filename,".Rout",sep=""))
    output.results()
    sink()
  }

  ### return the results (summary matrix)
  return(summaries)
}
