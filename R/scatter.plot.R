#' Quick Scatterplot Function for Exploratory Analyses
#'
#'  Function to make many scatterplots for exploratory analyses. Includes Pearson correlation and sample size in the plot title.
#' @param yname name of variable from tdat to plot on y-axis
#' @param xname name of variable from tdat to plot on x-axis
#' @param long.yname character string to be put on y-axis
#' @param long.xname character string to be put on x-axis
#' @param showr TRUE means include correlation and n on plot heading
#' @param use.pch pch symbol to be used in plot
#' @param use.cex cex (character expansion) for plotting symbols
#' @param jitx factor component of jitter(x,factor=jitx)
#' @param jity factor component of jitter(y,factor=jity)
#' @param addtomain additional text for main title (if showr=FALSE, this is the entire title)
#' @param cex.xylab cex for x and y axes
#' @param cex.title cex for title (and correlation, if shown)
#' @param ... Other graphical parameters sent to plot
#' @keywords scatterplot scatter
#' @export
#' @examples
#' test.dat<-data.frame(x=rnorm(10),y=rbinom(10,2,.5))
#' scatter.plot(test.dat,yname="y",xname="x")


scatter.plot <- function(tdat,yname,xname,
                         long.yname=NULL,long.xname=NULL,
                         showr=TRUE,use.pch=1,use.cex=1,
                         jitx=0,jity=0, addtomain=NULL,
                         cex.xylab=1.5,cex.title=1.2,...) {

  if ((yname %in% colnames(tdat))==FALSE) stop("yname does not match")
  if ((xname %in% colnames(tdat))==FALSE) stop("xname does not match")

  ### Create matrix of complete data for plot
  OK.nomiss <- !is.na(tdat[,yname]) & !is.na(tdat[,xname])
  complete.dat <- tdat[OK.nomiss,]
  ### Count number of obs with non-missing values for x and y
  nobs <- sum(OK.nomiss)
  ### and calculate correlation
  xycor <- cor(complete.dat[,yname],complete.dat[,xname])

  ### Define ylab and xlab, based on whether there are long names for x, y
  if (is.null(long.yname)) tylab <- yname else tylab <- long.yname
  if (is.null(long.xname)) txlab <- xname else txlab <- long.xname

  ### Check for validity of use.pch: if not valid, make pch=1
  if (is.numeric(use.pch) & (use.pch > 25)) use.pch=1

  ### Create plot
  plot(y=complete.dat[,yname],x=complete.dat[,xname],ylab=tylab,xlab=txlab,type="n",cex.lab=cex.xylab,...)
  points(y=jitter(complete.dat[,yname],factor=jity),x=jitter(complete.dat[,xname],factor=jitx),pch=use.pch,cex=use.cex)

  ### If include correlation and n, add to plot title
  if (is.null(addtomain)) {
    if (showr) title(paste("r = ",round(xycor,2)," (n=",nobs,")",sep=""),cex.main=cex.title)
  } else {
    if (showr) {
      title(paste(addtomain," r = ",round(xycor,2)," (n=",nobs,")",sep=""),cex.main=cex.title)
    } else {
      title(addtomain,cex.main=cex.title)
    }
  }
 }
