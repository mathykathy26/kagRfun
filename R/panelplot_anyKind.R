#' Creation of a Matrix of Many Plots for Exploratory Analyses
#'
#' Creates one to a large number of exploratory plots, where
#' the user has control over how the subplots (panels) are arranged
#' on the plots, without needing to keep track of how many plots
#' are created.  The function can be used to display the plots on
#' the screen, pausing between each plot, or to save the plots
#' to either pdf of eps files.  If the latter, the user also has
#' the option to create a simple LaTeX fragment which can be included
#' in LaTeX code to show all the plots created.  The scatter plots
#' by default show the correlation and sample size on the plot titles
#' (the user can change this).  By giving the relevant argument for
#' lookup.vec, this function will call on lookup to display the longer
#' names for the x- and y-axes.
#' There are 2 primary situations in which one might want to call
#' panelplot: (1) when we want to plot one or more outcome variables
#' (ynames) versus one or more covariates (xnames); and (2) when we
#' want to make all unique plots.  The latter is similar to pairs(),
#' but it doesn't include duplicate plots, the user has control over
#' how many plots to place on a page, and the plots can contain the
#' correlations and number of observations on the plot title.
#' @param tdat dataframe with variables for y-axes,
#' @param ynames names of variables to go on y-axis
#' @param xnames names of variables to go on x-axis
#' @param tnrows number of rows of plots
#' @param tncols number of columns of plots
#' @param plotmain title to go at top of each page of plots
#' @param uniqueplots TRUE means only make unique plots
#' @param skipSameXY TRUE means skip plots that have the same X and Y variable; only applies when uniqueplots=FALSE
#' @param save.plots TRUE means to save plots in a file
#' @param type.plots "pdf"  to create pdf files (only if save.plots=T), "eps" to create eps files
#' @param latex TRUE means create LaTeX code to show plots, and have includegraphics line
#' @param full.latex TRUE if want latex output to be a stand-alone file
#' @param basename base name of saved plots
#' @param lookup.vec vector of longer names, where column names should match variable names in tdat
#' @param showr TRUE to include correlation and n in plot heading
#' @param use.pch pch for plotting (in scatter.plot)
#' @param use.cex cex for plotting (in scatter.plot)
#' @param wait.between TRUE if want to wait between screens of plots
#' @param ... Other graphical parameters sent to plot
#' @keywords scatterplot scatter layout
#' @export
#' @examples
#' test.dat<-data.frame(x=rnorm(10),y=rbinom(10,2,.5))
#' panelplot.anyKind(test.dat)
#'
#' test.dat<-data.frame(x=1:10+rnorm(10),y=as.factor(c(rep(1,5),rep(0,5))))
#' panelplot.anyKind(test.dat)



###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### This function was created so that the user could create a large
### number of scatter plots (optionally including correlation and n in
### the plot title), without needing to hand code each plot.
###
### The function takes as required arguments the name of the
### dataframe in which the variables reside (tdat), and the
### names of the y-variables (ynames) and x-variables (xnames)
### for the plots to be made.
###
### By including a vector of longer variable names (lookup.vec) for which
### the names corresponding to the variable names in tdat, this function
### will include the longer names on the plot axes, by calling on lookup.
###
### There are 2 primary situations in which one might want to call
### panelplot: (1) when we want to plot one or more outcome variables
### (ynames) versus one or more covariates (xnames); and (2) when we
### want to make all unique plots.  The latter is similar to pairs(),
### but it doesn't include duplicate plots, the user has control over
### how many plots to place on a page, and the plots can contain the
### correlations and number of observations on the plot title.
###
### The user has the option to either view the plots on the screen
### (save.plots=FALSE, which is the default), or to save the plots
### (save.plots=TRUE) either to pdf files (type.plots="pdf", the default)
### or as eps files (type.plots="eps").   The user controls the base name
### of the plots with the basename= argument, so if basename=silly  and
### type.plots="pdf" then the plots will be silly1.pdf, silly2.pdf, etc.
###
### Also the user has the option of outputing crude LaTeX code, which
### has \includegraphics{plotname} for each plot being made.
### This can be particularly useful if one wants to make many plots
### without keeping track of how many plots are being made.

panelplot.anyKind <- function(tdat,
                      ynames=colnames(tdat), xnames=colnames(tdat),
                      tnrows=2,tncols=2,
                      maxperpage=tnrows*tncols,
                      new.page.x.var=FALSE,
                      new.page.y.var=FALSE,
                      plotmain=NULL,
                      uniqueplots=FALSE,
                      skipSameXY=FALSE,
                      save.plots=FALSE,
                      type.plots="pdf",
                      latex=FALSE,
                      full.latex=FALSE,
                      basename=NULL,
                      lookup.vec=NULL,
                      showr=TRUE,
                      use.pch=1,
                      use.cex=1,
                      wait.between=FALSE,...) {

  ### REQUIRED arguments are
  ###            tdat = dataframe with variables for y-axes,
  ###            ynames = names of variables to go on y-axis
  ###            xnames = names of variables to go on x-axis
  ### OPTIONAL arguments are
  ###            tnrows = number of rows of plots
  ###            tncols = number of columns of plots
  ###            plotmain = title to go at top of each page of plots
  ###            uniqueplots = TRUE means only make unique plots
  ###            skipSameXY = TRUE means skip plots that have the same X and Y variable; only applies when uniqueplots=FALSE
  ###            save.plots = TRUE means to save plots in a file
  ###            type.plots = "pdf"  to create pdf files (only if save.plots=T)
  ###                       = "eps" to create eps files
  ###            latex = TRUE means create LaTeX code to show plots, and have includegraphics line
  ###            full.latex=TRUE if want latex output to be a stand-alone file
  ###            basename   = base name of saved plots
  ###            lookup.vec = vector of longer names, where column names
  ###                         should match variable names in tdat
  ###            showr      = TRUE to include correlation and n in plot heading
  ###            use.pch    = pch for plotting (in scatter.plot)
  ###            use.cex    = cex for plotting (in scatter.plot)
  ###            linux      = TRUE if using R in linux
  ###            wait.between = TRUE if want to wait between screens of plots
  ###            dirname = directory pathway for sourcing functions

  ### if testing, print out some diagnostics later on
  testing <- FALSE

  ### First check to see that ynames and xnames are valid
  for (i in 1:length(ynames)) {
    if ((ynames[i] %in% colnames(tdat))==FALSE) stop("ynames does not match")
  }

  for (i in 1:length(xnames)) {
    if ((xnames[i] %in% colnames(tdat))==FALSE) stop("xnames does not match")
  }

  ###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  next.plot <- function(firstplot=FALSE) {

    ### This function first closes current device, if firstplot=TRUE,
    ### then opens file for next plot, if save.plots=TRUE.
    ### Also if make.latex, print relevant line to the output file

    if (!firstplot)  {
      if (!is.null(plotmain)) mtext(plotmain,outer=T,cex=1.5)
      if (save.plots) dev.off() #cat("would dev.off() here\n")
      if (wait.between) wait()
      #      if (save.plots & latex) cat("\n")
    }

    ### If saving plots, start next plot
    if (save.plots)  {
      if (type.plots=="eps") ending <- ".eps" else ending <- ".pdf"
      fname <- (paste(basename,plotctr,ending,sep=""))
      if (type.plots=="eps") postscript(fname) else pdf(fname)
      ### if latex, print line to be able to include plot in LaTeX
      if (latex)  {
        cat(" \n")
        cat("\n \n")
        cat("\n \\includegraphics{",fname,"}\n\n",sep="")
        #        cat("\n")
      }
    }
    par(mfrow=c(tnrows,tncols))
    if ((tnrows <= 2) & (tncols <= 2)) par(mar=c(4.1,4.1,2.1,2.1)) else par(mar=c(4.1,4.1,2.1,2.1))
    if (!is.null(plotmain)) par(oma=c(0,0,2,0)) else par(oma=c(0,0,0,0))
  }  ### end of next.plot()

  ###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ### Set up counters:
  plotctr <- 1                 # the plot counter
  npanels <- min(tnrows * tncols,maxperpage)   # number of panels per plot
  panelctr <- 1                # counter for panel (subplot) within the plot
  unique.ctr <- 1              # total number of panels (subplots)

  ### if user wants to save plots but didn't give basenames, create one
  if (is.null(basename)) basename <- "panelplot"

  ### if full.latex, sink output into a file to be returned at end
  if (full.latex) sink(paste(basename,".Rout",sep=""))

  ### Start a new plot
  next.plot(first=TRUE)

  ###***************************************
  ### START MAIN PART OF PROGRAM
  ### IF user wants all unique plots,

  if (uniqueplots) {
    unique.names <- unique(c(xnames,ynames))
    if (testing) { cat("unique.names=\n");  print(unique.names) }
    length.names <- length(unique.names)

    ### Figure out how many panels to make
    n.unique <- length.names * (length.names - 1) / 2
    unique.vars <- tdat[,unique.names]

    for (i in 1:(length(unique.names)-1)) {
      for (j in (i+1):length(unique.names)) {
        tylab <- unique.names[i]
        txlab <- unique.names[j]

        ### call on scatter.plot to create plot
        scatter.box.mosaic.plot(tdat,tylab,txlab,lookup(tylab,lookup.vec),
                                lookup(txlab,lookup.vec),showr=showr,
                                use.pch=use.pch,use.cex=use.cex,...)

        if (testing) cat("y-axis=",tylab," x-axis=",txlab,"plotctr=",plotctr,"panelctr=",panelctr,"\n")

        panelctr <- panelctr + 1
        unique.ctr <- unique.ctr + 1

        ### update plotctr and panelctr if have made all subplots on the page
        ### AND there are still plots to be made

        if ((panelctr > npanels) & (unique.ctr < n.unique)) {
          ### update plotctr, also set panelctr to 1

          panelctr <- 1
          plotctr <- plotctr + 1
          next.plot()
        }
      }
    }


  } else {

    ### ************************************************
    ### User does not want all unique plots, but instead
    ### wants each element of ynames plotted vs each element of xnames

    n.unique <- length(ynames) * length(xnames)

    ### Define yvars and xvars
    yvars <- tdat[,ynames]
    xvars <- tdat[,xnames]

    for (i in 1:ncol(as.matrix(yvars))) {
      for (j in 1:ncol(as.matrix(xvars))) {

        tylab <- ynames[i]
        txlab <- xnames[j]

        if((tylab==txlab) & skipSameXY) next

        new.x<-ifelse(j>1,txlab==xnames[j-1],F)

        ### call on scatter.plot to create plot
        scatter.box.mosaic.plot(tdat,tylab,txlab,lookup(tylab,lookup.vec),
                                lookup(txlab,lookup.vec),showr=showr,
                                use.pch=use.pch,use.cex=use.cex,...)

        if (testing) cat("y-axis=",tylab," x-axis=",txlab,"plotctr=",plotctr,"panelctr=",panelctr,"\n")

        panelctr <- panelctr + 1
        unique.ctr <- unique.ctr + 1

        ### update plotctr and panelctr if have made subplots on the page
        ### update plotctr and panelctr if have made all subplots on the page
        ### AND there are still plots to be made
        if ((panelctr > npanels) & (unique.ctr < n.unique)) {
          ### update plotctr, also set panelctr to 1
          panelctr <- 1
          plotctr <- plotctr + 1
          next.plot()
        }
      }
      if(new.page.y.var){
        ### update plotctr, also set panelctr to 1
        panelctr <- 1
        plotctr <- plotctr + 1
        next.plot()
      }
    }

  }

  if (!is.null(plotmain)) mtext(plotmain,outer=T,cex=1.5)

  if (save.plots) dev.off()
  if (wait.between) wait()

  ### if full.latex, stop sinking
  if (full.latex) {
    sink()
  }
}


