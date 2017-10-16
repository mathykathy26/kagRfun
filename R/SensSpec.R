#' Sensitivity and Specificity
#'
#'  Function to calculate a table of sensitivities, specficities and error rates for classification procedures using predicted probabilities and true classification labels.
#' @param x Vector of predicted probabilities for classification. For example, the predicted probabilities from a logistic model.
#' @param grouping The vector of true class labels for the data.
#' @param thresh Vector of desired thresholds for classification. The table will have as many rows as thresholds.
#' @param plots Logical, plot the sensitivity, specificity and error rate curves versus the threshold vector on one plot.
#' @param table Logical, returns the table of sensitivities, specificities and error rates for all thresholds.
#' @param legend.loc Vector of two values for the x and y coordinate of the legend, respectively. Applicable only when plot is TRUE.
#' @keywords sensitivity specificity error classification
#' @export
#' @examples
#' test.dat<-data.frame(x=rnorm(10),y=rbinom(10,2,.5))
#' SensSpec(test.dat$x,test.dat$y)
#' SensSpec(test.dat$x,test.dat$y,plot=FALSE,tab=TRUE)

SensSpec<-function(x,
                   grouping,
                   thresh=seq(0,1,.05),
                   plots=T,
                   table=F,
                   legend.loc=c(.7*max(thresh),.85)){
  if(!is.numeric(grouping)) grouping<-as.numeric(grouping)-1

  tab<-array(,dim=c(length(thresh),4))
  for(c in 1:length(thresh)){
    diagnosis<-as.numeric(1*(x>=thresh[c]))
    tab[c,1]<-sum(diagnosis==1 & grouping==1)/sum(grouping==1)
    tab[c,2]<-sum(diagnosis==0 & grouping==0)/sum(grouping==0)

    tab[c,3]<-mean(((diagnosis==1 & grouping==0)|(diagnosis==0 & grouping==1)))
  }
  tab[,4]<-thresh
  colnames(tab)<-c("Sensitivity","Specificity","Error Rate","Threshold")

  if(plots){
    best<-thresh[which(tab[,3]==min(tab[,3]))]
    if(length(best)==1) Main<-paste0("Minimum Error Rate: ",round(min(tab[,3]),3)*100,"% at c=",best)
    if(length(best)>1)  Main<-paste0("Minimum Error Rate: ",round(min(tab[,3]),3)*100,"% at c=(",min(best),",",max(best),")")

    plot(range(thresh),c(0,1),type="n",xlab="Threshold",ylab="Proportion",
         main=Main)
    lines(thresh,tab[,1],col=3,lwd=2)
    lines(thresh,tab[,2],col=4,lwd=2)
    lines(thresh,tab[,3],col=2,lwd=2)
    if(is.character(legend.loc)) legend(legend.loc,cex=.75,legend=c("Sensitivity","Specificity","Error Rate"),col=c(3,4,2),lwd=2)
    if(is.numeric(legend.loc)) legend(x=legend.loc[1],y=legend.loc[2],cex=.75,legend=c("Sensitivity","Specificity","Error Rate"),col=c(3,4,2),lwd=2)
  }

  if(table) return(tab)
}
