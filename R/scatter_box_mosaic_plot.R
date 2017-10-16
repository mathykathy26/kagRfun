#' Quick Adaptive Plotting Function for Exploratory Analyses
#'
#'  Function to make many scatterplots, boxplots and mosaic plots as necessary for exploratory analyses. Includes the Spearman correlation coefficient and sample size in the plot title. Correlation coefficients for plots with any categorical variables are calculated after converting the factors to numeric. The titles are printed in red if the correlation is significant. Significance is tested by the method appropriate to the variable: scatterplots use Spearman's test, boxplots use Kruskal-Wallis and mosaics use Chi-Square test.
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
#' scatter.box.mosaic.plot(test.dat,yname="y",xname="x")
#'
#' test.dat<-data.frame(x=1:10+rnorm(10),y=as.factor(c(rep(1,5),rep(0,5))))
#' scatter.box.mosaic.plot(test.dat,yname="y",xname="x")


scatter.box.mosaic.plot <- function(tdat,yname,xname,long.yname=NULL,long.xname=NULL,
                                showr=TRUE,use.pch=1,use.cex=1,jitx=0,jity=0,
                                addtomain=NULL,cex.xylab=1.5,cex.title=1.2,...) {


  if ((yname %in% colnames(tdat))==FALSE) stop("yname does not match")
  if ((xname %in% colnames(tdat))==FALSE) stop("xname does not match")

  ### Create matrix of complete data for plot
  OK.nomiss <- !is.na(tdat[,yname]) & !is.na(tdat[,xname])
  complete.dat <- tdat[OK.nomiss,]
  ### Count number of obs with non-missing values for x and y
  nobs <- sum(OK.nomiss)
  ### and calculate correlation
  if(nobs>0){
    if(is.factor(complete.dat[,yname]) | is.factor(complete.dat[,xname])){
      xycor <- suppressWarnings(cor(as.numeric(complete.dat[,yname]),as.numeric(complete.dat[,xname]),method="spearman"))
      if(is.factor(complete.dat[,yname]) & !is.factor(complete.dat[,xname])){
        if(length(levels(complete.dat[,yname]))>2)   xyp <- kruskal.test(complete.dat[,xname]~complete.dat[,yname])$p.value
        if(length(levels(complete.dat[,yname]))==2)  xyp <- suppressWarnings(wilcox.test(complete.dat[,xname]~complete.dat[,yname])$p.value)
        if(length(levels(complete.dat[,yname]))==1)  xyp <- NA
      }
      if(!is.factor(complete.dat[,yname]) & is.factor(complete.dat[,xname])){
        if(length(levels(complete.dat[,xname]))>2)   xyp <- kruskal.test(complete.dat[,yname]~complete.dat[,xname])$p.value
        if(length(levels(complete.dat[,xname]))==2)  xyp <- suppressWarnings(wilcox.test(complete.dat[,yname]~complete.dat[,xname])$p.value)
        if(length(levels(complete.dat[,xname]))==1)  xyp <- NA
      }
      if(is.factor(complete.dat[,yname]) & is.factor(complete.dat[,xname])){
        xyp<-suppressWarnings(chisq.test(table(complete.dat[,yname],complete.dat[,xname]),simulate.p.value=F)$p.value)
      }
    }else{
      xycor <- suppressWarnings(cor(complete.dat[,yname],complete.dat[,xname],method="spearman"))
      xyp <- suppressWarnings(cor.test(complete.dat[,yname],complete.dat[,xname],method="spearman",exact=T)$p.value)
    }
  }

  ### Define ylab and xlab, based on whether there are long names for x, y
  if (is.null(long.yname)) tylab <- yname else tylab <- long.yname
  if (is.null(long.xname)) txlab <- xname else txlab <- long.xname

  ### Check for validity of use.pch: if not valid, make pch=1
  if (is.numeric(use.pch) & (use.pch > 25)) use.pch=1

  ### Create plot
  if(nobs==0){
    plot(c(0,1),c(0,1),ylab=tylab,xlab=txlab,type="n",cex.lab=cex.xylab)
    xycor=xyp=NA
  }else{
    if((is.numeric(complete.dat[,yname]) & is.numeric(complete.dat[,xname]))|
       (is.factor(complete.dat[,yname]) & is.factor(complete.dat[,xname]))){
      plot(y=complete.dat[,yname],x=complete.dat[,xname],ylab=tylab,xlab=txlab,cex.lab=cex.xylab)
      # if(is.numeric(complete.dat[,yname]) & is.numeric(complete.dat[,xname]))
        # points(y=jitter(complete.dat[,yname],factor=jity),x=jitter(complete.dat[,xname],factor=jitx),pch=use.pch,cex=use.cex)
    }
    if(is.factor(complete.dat[,yname]) & !is.factor(complete.dat[,xname])){
      boxplot(complete.dat[,xname]~complete.dat[,yname],ylab=tylab,xlab=txlab,cex.lab=cex.xylab,horizontal=T)
      if(length(levels(complete.dat[,yname]))==1) axis(1,at=1,labels=levels(complete.dat[,yname]))
    }
    if(!is.factor(complete.dat[,yname]) & is.factor(complete.dat[,xname])){
      boxplot(complete.dat[,yname]~complete.dat[,xname],ylab=tylab,xlab=txlab,cex.lab=cex.xylab)
      if(length(levels(complete.dat[,xname]))==1) axis(1,at=1,labels=levels(complete.dat[,xname]))
    }
  }
  ### If include correlation and n, add to plot title
  if (is.null(addtomain)) {
    if (showr) title(paste("r = ",round(xycor,2)," (n=",nobs,") p=",round(xyp,2),sep=""),cex.main=cex.title,col.main=ifelse(xyp<.05,2,1))
  } else {
    if (showr) {
      title(paste(addtomain," r = ",round(xycor,2)," (n=",nobs,") p=",round(xyp,2),sep=""),cex.main=cex.title,col.main=ifelse(xyp<.05,2,1))
    } else {
      title(addtomain,cex.main=cex.title)
    }
  }
}
