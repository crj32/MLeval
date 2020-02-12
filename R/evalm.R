if(getRversion() >= "2.15.1")  utils::globalVariables(c('Group','PG','PREC','RG','SENS','predmean',
                                                        'realmean'))

#' evalm: Evaluate Machine Learning Models in R
#'
#' evalm is for machine learning model evaluation in R. The function can accept the Caret 'train' 
#' function results to evaluate machine learning predictions or a data frame of probabilities and 
#' ground truth labels can be passed in to evaluate. Probability data must be column1: probability 
#' group1 (column named as your group name 1), column2: probability group2 (column named as your group name 2), 
#' column3: observation labels (column named 'obs'), column4: Group, e.g. different models (column
#' named 'Group'), optional to include if different models are combined horizontally.
#'
#' @param list1 List or data frame: List of Caret results objects from train, or a single train results object, or a data frame of probabilities and observed labels
#' @param gnames Character vector: A vector of group names for the fit objects 
#' @param title Character string: A title for the ROC plot 
#' @param cols Character vector: A vector of colours for the group or groups 
#' @param rlinethick Numerical value: Thickness of the ROC curve line
#' @param fsize Numerical value: Font size for the ROC curve plots
#' @param dlinecol Character string: Colour of the diagonal line
#' @param optimise Character string: Metric by which to select the operating point (INF, MCC, or F1)
#' @param positive Character string: Name of the positive group (will effect PR metrics)
#' @param dlinethick Numerical value: Thickness of the diagonal line
#' @param bins Numerical value: Number of bins for calibration curve
#' @param showplots Logical flag: whether to show plots or not
#' @param plots Character vector: which plots to show: r = roc, pr = proc, prg = precision recall gain, cc = calibration curve 
#' @param percent Numerical value: percentage for the confidence intervals (default = 95)
#' @param silent Logical flag: whether to hide messages (default=FALSE)
#' 
#' @return 
#' List containing: 1) A ggplot2 ROC curve object for printing
#' 2) A ggplot2 PROC object for printing
#' 3) A ggplot2 PRG curve for printing
#' 4) Optimised results according to defined metric
#' 5) P cut-off of 0.5 standard results
#' @export
#'
#' @examples
#' r <- evalm(fit)
evalm <- function(list1,gnames=NULL,title='',cols=NULL,silent=FALSE,
                 rlinethick=1.25,fsize=12.5,dlinecol='grey',
                 dlinethick=0.75,bins=6,optimise='INF',percent=95,
                 showplots=TRUE,positive=NULL,plots=c('prg','pr','r','cc')){
  
  if(silent==FALSE){
    message('***MLeval: Machine Learning Model Evaluation***')
  }
  
  ## if not a list convert to list
  if (class(list1)[1] != 'list'){
    list1 <- list(list1)
    list1b <- list1
  }
  
  ## read whether input is data frame or caret object
  if (class(list1[[1]])[1] == 'train'){
    input <- 'caret'
    if(silent==FALSE){
      message('Input: caret train function object')
    }
    ## decide whether to average over reps or not
    if (list1[[1]]$control$method == 'cv' | list1[[1]]$control$method == 'LOOCV'){
      mode <- 'cv'
      if(silent==FALSE){
        message('Not averaging probs.')
      }
    }else{
      mode <- 'rep'
      if(silent==FALSE){
        message('Averaging probs.')
      }
    }
  }else if (class(list1[[1]])[1] == 'data.frame'){
    input <- 'normal'
    if(silent==FALSE){
      message('Input: data frame of probabilities of observed labels')
    }
  }else{
    stop('Data frame or Caret train object required please.')
  }
  
  ## if w/o group names, make these
  if (is.null(gnames) & input == 'caret'){
    gnames <- c()
    #
    for (dt in seq(1,length(list1))){
      gnames <- c(gnames,paste('Group',dt))
    }
  }else if (is.null(gnames) & input == 'normal'){
    gnames <- levels(as.factor(list1[[1]]$Group))
  }
  
  ## if not caret get group names here
  if (input == 'normal'){
    df <- list1[[1]]
    names <- colnames(df)[1:2]
  }
  
  ## if not custom colours then use this palette
  if (is.null(cols)){
    cols <- c("red","slateblue","grey","gold","orange","forestgreen", 
               "violetred","violet","skyblue","darkorchid")
  }
  
  ## error handling
  if ( is.null(list1[[1]]$pred) & input == 'caret') {
    stop("No probabilities found in Caret output")
  }
  
  ## set positive names manually if necessary
  if (is.null(positive) == FALSE){
    if (input == 'caret'){
      names <- as.character(list1[[1]]$levels)
      if(which(names==positive)==1){
        namesr <- rev(names)
      }else{
        namesr <- names
      }
    }else if (input == 'normal'){
      names <- names
      if(which(names==positive)==1){
        namesr <- rev(names)
      }else{
        namesr <- names
      }
    }
  }
  
  ## average probabilities if this is a caret object
  if (input == 'caret'){
    
    ## diagnostics for input object
    for (dt in seq(1,length(list1))){
      fit1z <- list1[[dt]]
      if(silent==FALSE){
        message(paste('Group',dt,'type:',fit1z$control$method))
      }
    }
    
    ## for caret fit object input
    names <- as.character(list1[[1]]$levels)
    gnames <- as.factor(gnames)
    
    if (is.null(positive) == FALSE){
      names <- namesr
    }
    G1 <- names[1]
    G2 <- names[2]
    
    ## start caret code (get optimal indices, get mean of repeated cv, combine data)
    list2 <- list()
    ## get caret optimal parameter indices
    for (dt in seq(1,length(list1))){
      fit1z <- list1[[dt]]
      mind <- list()
      for (ii in seq(1,length(fit1z$bestTune))){
        pp <- names(fit1z$bestTune)[ii]
        toadd <- which(fit1z$pred[[pp]] == fit1z$bestTune[[ii]])
        mind[[ii]] <- toadd
      }
      # keep only those indices matching for all parameters
      mind <- Reduce(intersect, mind)
      list2[[dt]] <- mind
    }
    
    ## get data out
    if (mode == 'rep'){
      # start KF
      myl <- list()
      for (dt in seq(1,length(list1))){ # for each fit in list
        ## get data 1
        output <- list1[[dt]]
        indices <- list2[[dt]]
        # code to get a mean series of probabilities out
        preds <- output$pred[indices, ]
        preds <- preds[,c('obs',G1,G2,'rowIndex','Resample')]
        preds$replicates <- sapply(strsplit(preds$Resample,'\\.'), `[`, 2)
        ## resamples is a list of data frames each with an individual sample's probabilities
        ## in for all resamples
        resamples <- split(preds,preds$rowIndex)
        finalres <- matrix(nrow=length(resamples),ncol=4)
        for (i in seq(1,length(resamples))){
          dfx <- resamples[[i]]
          finalres[i,1] <- dfx[1,4] # row index
          finalres[i,2] <- mean(dfx[[G1]]) # mean group 1
          finalres[i,3] <- mean(dfx[[G2]]) # mean group 2
          finalres[i,4] <- as.character(dfx[1,1])
        }
        finalres <- data.frame(finalres)
        finalres$X2 <- as.numeric(as.character(finalres$X2))
        finalres$X3 <- as.numeric(as.character(finalres$X3))
        finalres <- finalres[,c(2,3,4)] # select G1, G2, obs
        colnames(finalres) <- c(G1,G2,'obs')
        finalres$obs <- as.character(finalres$obs)
        finalres$obs <- as.factor(finalres$obs)
        finalres$Group <- gnames[dt]
        #
        myl[[dt]] <- finalres
      }
      # end KF
    }else if (mode == 'cv') {
      # start loocv/ cv
      myl <- list()
      for (dt in seq(1,length(list1))){ # for each fit in list
        ## get data 1
        output <- list1[[dt]]
        indices <- list2[[dt]]
        preds <- output$pred[indices, ]
        finalres <- preds[c(G1,G2,'obs')]
        finalres$obs <- as.character(finalres$obs)
        finalres$obs <- as.factor(finalres$obs)
        finalres$Group <- gnames[dt]
        myl[[dt]] <- finalres
      }
    }
    ### bind them
    fres <- matrix(ncol=4,nrow=0)
    for (dt in seq(1,length(list1))){
      dd <- myl[[dt]]
      fres <- rbind(fres,dd)
    }
    finalres <- fres
    ## end caret code
  }else if (input == 'normal'){
    ## for a non caret input
    # column1: prob G1
    # column2: prob G2
    # column3: obs labels
    # column4: Group (optional)
    # get obs labels
    if (is.null(positive) == FALSE){
      names <- namesr
    }
    G1 <- names[1]
    G2 <- names[2]
    # process
    finalres <- list1[[1]]
    colnames(finalres)[3] <- 'obs'
    finalres$obs <- as.character(finalres$obs)
    finalres$obs <- as.factor(finalres$obs)
    if("Group" %in% colnames(finalres)){
      if(silent==FALSE){
        message('Group column exists.')
      }
    }else{
      finalres$Group <- 'Group1'
      gnames <- factor(c('Group1'))
      if(silent==FALSE){
        message('Group does not exist, making column.')
      }
    }
  }
  
  #dim(finalres)
  #print(finalres$Group)
  #Sys.sleep(100000)
  
  if(silent==FALSE){
    ## sample probabilities diagnostics
    message(paste('Observations:',nrow(finalres)))
    message(paste('Number of groups:',length(gnames)))
    message(paste('Observations per group:',nrow(finalres)/length(gnames)))
    
    ### inform which is positive and negative
    message(paste('Positive:',G2))
    message(paste('Negative:',G1))
  }
  
  #print(finalres$Group)
  gszp<-c()
  gszn<-c()
  ## get N of each group
  for (group in seq(1,length(gnames))){
    #print(gnames[group])
    #print(finalres$Group)
    finalres2 <- subset(finalres, finalres$Group == as.character(gnames[group]))
    #print(head(finalres2))
    if(silent==FALSE){
      message(paste('Group:',as.character(gnames[group])))
      message(paste('Positive:',sum(as.character(finalres2$obs)==G2)))
    }
    gszp[group] <- sum(as.character(finalres2$obs)==G2)
    if(silent==FALSE){
      message(paste('Negative:',sum(as.character(finalres2$obs)==G1)))
    }
    gszn[group] <- sum(as.character(finalres2$obs)==G1)
  }
  
  ## calculate metrics for each threshold do PR and ROC
  finalres$Group <- as.character(finalres$Group)
  aucs <- c()
  aucprs <- c()
  rocm <- NULL # hold the roc curves for each group
  # loop over to get multiple rocs from different groups
  for (group in seq(1,length(gnames))){
    finalres2 <- subset(finalres, finalres$Group == gnames[group])
    ## calculate ml metrics
    mlout <- mlmetrics(finalres2,G1=G1,G2=G2)
    ## append the 0,0 and 1,1 coordinates
    SPECi <- c(0,1-mlout$SPEC)
    SENSi <- c(0,mlout$SENS)
    ## calculate AUC
    auc <- AUCc(SPECi,SENSi,method = c("trapezoid"))
    aucpr <- AUCc(mlout$SENS,mlout$PREC,method = c("trapezoid"))
    ##
    aucs <- c(aucs,round(auc,2))
    aucprs <- c(aucprs,round(aucpr,2))
    rocm <- rbind(rocm, mlout)
  }
  rocm <- data.frame(rocm)
  
  ## reformatting
  rocm[[G1]] <- as.numeric(as.character(rocm[[G1]]))
  rocm[[G2]] <- as.numeric(as.character(rocm[[G2]]))
  # rocm$SENS <- as.numeric(as.character(rocm$SENS))
  # rocm$SPEC <- as.numeric(as.character(rocm$SPEC))
  # rocm$Informedness <- as.numeric(as.character(rocm$Informedness))
  # rocm$PREC <- as.numeric(as.character(rocm$PREC))
  # rocm$NPV <- as.numeric(as.character(rocm$NPV))
  # rocm$F1 <- as.numeric(as.character(rocm$F1))
  rocm$FPR <- 1-rocm$SPEC
  
  ## plot ROC curves on one plot
  gnames <- as.character(gnames)
  llabels <- paste(gnames,c('\n AUC-ROC =','\n AUC-ROC ='),aucs)
  llabels2 <- paste(gnames,c('\n AUC-PR =','\n AUC-PR ='),aucprs)
  
  #
  rocm$Group <- factor(rocm$Group, levels = gnames)
  
  ## prg calculations
  cc<-NULL
  aucprgs <- c()
  for (dt in seq(1,length(gnames))){
    rocmt <- subset(rocm, rocm$Group==gnames[dt])
    # PRG computations 
    # https://papers.nips.cc/paper/5867-precision-recall-gain-curves-pr-analysis-done-right.pdf
    P <- sum(rocmt$obs==G2)
    PN <- length(rocmt$obs)
    bl <- P/PN
    rocmt$PG <- (rocmt$PREC-bl)/(1-bl)*rocmt$PREC
    rocmt$RG <- (rocmt$SENS-bl)/(1-bl)*rocmt$SENS
    rocmt$RG[rocmt$RG<0] <- 0
    rocmt$PG[rocmt$PG<0] <- 0
    # AUC
    aucprg <- AUCc(rocmt$PG,rocmt$RG,method = c("trapezoid"))
    aucprg <- round(aucprg,2)
    aucprgs <- c(aucprgs,aucprg)
    #
    cc<-rbind(cc,rocmt)
  }
  llabels3 <- paste(gnames,c('\n AUC-PRG =','\n AUC-PRG ='),aucprgs)
  rocm<-cc
  
  ## set NA to zero
  rocm$NPV[is.na(rocm$NPV)]<-0
  rocm$PREC[is.na(rocm$PREC)]<-0
  
  #print(rocm)
  #print()
  #Sys.sleep(1000)
  
  ## end metric calcs
  if(silent==FALSE){
    message('***Performance Metrics***')
  }
  
  ## ggplot2 functions
  g <- mroc(rocm,title=title,cols=cols,
              rlinethick=rlinethick,fsize=fsize,dlinecol=dlinecol,
              dlinethick=dlinethick,llabels = llabels) 
  g2 <- proc(rocm,title=title,cols=cols,
               rlinethick=rlinethick,fsize=fsize,dlinecol=dlinecol,
               dlinethick=dlinethick,llabels = llabels2,G1=G2,
               G2=G2) 
  g3 <- prg(rocm,title=title,cols=cols,
              rlinethick=rlinethick,fsize=fsize,
              llabels=llabels3,G1=G2,
              G2=G2)
  g4 <- cc(rocm,title=title,cols=cols,
           rlinethick=rlinethick,fsize=fsize,
           llabels=gnames,G1=G2,
           G2=G2,bins=bins)
  
  ## show plots
  if (showplots){
    if ('cc' %in% plots){
      print(g4)
    }
    if ('pr' %in% plots){
      print(g2)
    }
    if ('prg' %in% plots){
      print(g3)
    }
    if ('r' %in% plots){ # goes last
      print(g)
    }
  }
  
  if (input == 'caret'){
    ## get group metrics and print
    probl <- list()
    ## split results by group
    for (dt in seq(1,length(list1))){
      probl[[dt]] <- subset(rocm, rocm$Group==gnames[dt])
    }
    names(probl) <- gnames
  }else if (input == 'normal'){ ### probl = problem atm
    ## get group metrics and print
    probl <- list()
    ## split results by group
    for (dt in seq(1,length(gnames))){
      probl[[dt]] <- subset(rocm, rocm$Group==gnames[dt])
    }
    names(probl) <- gnames
  }
  
  ## find and print optimised metrics
  ## extract optimised metrics for results
  ## extract p=0.5 metrics for results also
  optrl <- list()
  stdrl <- list()
  for (dt in seq(1,length(gnames))){
    temp <- probl[[dt]]
    ### get optimised performance (by metric X)
    if (optimise == 'INF'){
      if(silent==FALSE){
        message(paste(gnames[dt],'Optimal Informedness =',temp$Informedness[which.max(temp$Informedness)]))
      }
      optres <- temp[which.max(temp$Informedness),,drop=FALSE]
    }else if (optimise == 'MCC'){
      if(silent==FALSE){
        message(paste(gnames[dt],'Optimal MCC =',temp$MCC[which.max(temp$MCC)]))
      }
      optres <- temp[which.max(temp$MCC),,drop=FALSE]
    }else if (optimise == 'F1'){
      if(silent==FALSE){
        message(paste(gnames[dt],'Optimal F1 score =',temp$F1[which.max(temp$F1)]))
      }
      optres <- temp[which.max(temp$F1),,drop=FALSE]
    }
    optres <- optres[,c('SENS','SPEC','MCC','Informedness','PREC','NPV','FPR','F1',
                        'TP','FP','TN','FN')] # add here for new metrics
    optresb <- optres[,c('SENS','SPEC','MCC','Informedness','PREC','NPV','FPR','F1',
                        'TP','FP','TN','FN')] # add here for new metrics
    optres <- t(optres)
    optres <- data.frame(optres)
    colnames(optres)[1] <- 'Score'
    optres[,1] <- as.numeric(as.character(optres[,1]))
    new <- data.frame(Score=c(aucs[dt],aucprs[dt],aucprgs[dt]))
    ## AUC always go at end
    optres <- rbind(optres,new)
    s<-nrow(optres)-2
    e<-nrow(optres)
    row.names(optres)[s:e] <- c('AUC-ROC','AUC-PR','AUC-PRG') # bump for new metrics +1
    ## get CIs
    optres$CI <- NA
    #print(ciroc)
    optres[s,2] <- ciauc(optres[s,1],gszp[dt],gszn[dt],percent)
    optres['SENS',2] <- wci(optres['SENS',1],gszp[dt],percent)
    optres['SPEC',2] <- wci(optres['SPEC',1],gszn[dt],percent)
    optres['PREC',2] <- wci(optres['PREC',1],(optresb$TP+optresb$FP),percent)
    optres['NPV',2] <- wci(optres['NPV',1],(optresb$TN+optresb$FN),percent)
    #
    optres[,1] <- round(optres[,1],3)
    #
    optrl[[dt]] <- optres
    ### get p=0.5 performance (default)
    stdres <- mlmetricsb(temp,G1=G1,G2=G2)
    stdresb <- stdres
    stdres <- t(stdres)
    stdres <- data.frame(stdres)
    colnames(new) <- 'stdres'
    stdres <- rbind(stdres,new)
    # add CI to standard
    stdres$CI <- NA
    stdres['SENS',2] <- wci(stdres['SENS',1],gszp[dt],percent)
    stdres['SPEC',2] <- wci(stdres['SPEC',1],gszn[dt],percent)
    stdres['PREC',2] <- wci(stdres['PREC',1],(stdresb$TP+stdresb$FP),percent)
    stdres['NPV',2] <- wci(stdres['NPV',1],(stdresb$TN+stdresb$FN),percent)
    stdres[s,2] <- optres[s,2]
    colnames(stdres)[1] <- 'Score'
    row.names(stdres)[s:e] <- c('AUC-ROC','AUC-PR','AUC-PRG')
    stdres[,1] <- as.numeric(as.character(stdres[,1]))
    stdres[,1] <- round(stdres[,1],3)
    stdrl[[dt]] <- stdres
  }
  names(optrl) <- gnames
  names(stdrl) <- gnames
  
  ## reformatting
  for (dt in seq(1,length(gnames))){
    probl[[dt]] <- probl[[dt]][ , -which(names(probl[[dt]]) %in% c("predt"))]
  }
  
  ## print AUCs
  for (n in seq(1,length(gnames))){
    if(silent==FALSE){
      message(paste(gnames[n],'AUC-ROC =',aucs[n]))
    }
  }
  
  ## output
  return(list('roc'=g,'proc'=g2,'prg'=g3,'cc'=g4,'probs'=probl,'optres'=optrl,'stdres'=stdrl))
}

mlmetrics <- function(mlr,G1=G1,G2=G2){
  ## for each group compute...
  p <- mlr
  p <- p[order(-p[,G2]), ]
  i = 1
  for (val in p[[G2]]){
    p$predt <- NA
    p$predt[p[[G2]]<val] <- G1 # for cut-off if less than label as
    p$predt[p[[G2]]>=val] <- G2
    # do calcs
    pred.pos <- p$predt == G2
    pred.neg <- p$predt != G2
    truth.pos <- p$obs == G2
    truth.neg <- p$obs != G2
    p$TP[i] <- sum(pred.pos & truth.pos)
    p$TN[i] <- sum(pred.neg & truth.neg)
    p$FP[i] <- sum(pred.pos & truth.neg)
    p$FN[i] <- sum(pred.neg & truth.pos)
    i = i + 1
  }
  p$SENS <- p$TP/(p$TP + p$FN)
  p$SPEC <- p$TN/(p$TN + p$FP)
  p$Informedness <- p$SENS + p$SPEC - 1
  p$PREC <- p$TP/(p$TP + p$FP)
  p$NPV <- p$TN/(p$TN + p$FN)
  p$MARK <- p$PREC + p$NPV - 1
  p$F1 <- 2*p$PREC*p$SENS/(p$PREC + p$SENS)
  p$F1[is.na(p$F1)] <- 0
  p$MCC <- sign(p$Informedness)*sqrt(p$Informedness*p$MARK) # ((p$TP*p$TN)-(p$FP*p$FN)) / sqrt((p$TP+p$FP)*(p$TP+p$FN)*(p$TN+p$FP)*(p$TN+p$FN))
  # 10 column output
  return(p)
}

mlmetricsb <- function(mlr,G1=G1,G2=G2){
  #print(head(mlr))
  ## for each group compute...
  p <- mlr
  p <- p[order(-p[,G2]), ]
  p$predt[p[[G2]]<0.5] <- G1 # for p=0.5 cut-off
  p$predt[p[[G2]]>=0.5] <- G2
  pred.pos <- p$predt == G2
  pred.neg <- p$predt != G2
  truth.pos <- p$obs == G2
  truth.neg <- p$obs != G2
  TP <- sum(pred.pos & truth.pos)
  TN <- sum(pred.neg & truth.neg)
  FP <- sum(pred.pos & truth.neg)
  FN <- sum(pred.neg & truth.pos)
  SENS <- TP/(TP + FN)
  SPEC <- TN/(TN + FP)
  FPR <- 1-SPEC
  INF <- SENS + SPEC - 1
  PPV <- TP/(TP + FP)
  NPV <- TN/(TN + FN)
  F1 <- 2*PPV*SENS/(PPV + SENS)
  if (is.na(F1)){
    F1 <- 0
  }
  MARK <- PPV + NPV - 1
  MCC <- sign(INF)*sqrt(INF*MARK) # ((TP*TN)-(FP*FN)) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  stdres <- data.frame(SENS=SENS,SPEC=SPEC,MCC=MCC,Informedness=INF,
                       PREC=PPV,NPV=NPV,FPR=FPR,F1=F1,TP=TP,FP=FP,
                       TN=TN,FN=FN) # add here for new metrics must be in order
  # 
  return(stdres)
}

AUCc <- function(x, y, method = "trapezoid"){
  idx <- order(x)
  x <- x[idx]
  y <- y[idx]
  if (method == 'trapezoid'){
    auc <- sum((rowMeans(cbind(y[-length(y)], y[-1]))) * (x[-1] - x[-length(x)]))
  }else if (method == 'step'){
    auc <- sum(y[-length(y)] * (x[-1] - x[-length(x)]))
  }else if (method == 'spline'){
    auc <- integrate(splinefun(x, y, method = "natural"), lower = min(x), upper = max(x))
    auc <- auc$value
  }
  return(auc)
}

mroc <- function(rocm,title='',cols=NULL,
                 rlinethick=1.5,fsize=15,dlinecol='grey',
                 dlinethick=0.75,llabels = llabels){
  
  rocm2 <- NULL
  for (group in unique(rocm$Group)){
    rocmt <- subset(rocm,rocm$Group==group)
    ## add 0,0 co ordinates to ROC and 1,1
    FPR <- rocmt$FPR
    SENS <- rocmt$SENS
    FPR <- c(0,FPR,1)
    SENS <- c(0,SENS,1)
    rocmt <- data.frame(SENS=SENS,FPR=FPR,Group=group)
    rocm2 <- rbind(rocm2,rocmt)
  }
  
  ##
  g <- ggplot(rocm2, aes(x = FPR, y = SENS, color = Group)) + 
    geom_line(size = rlinethick) +
    geom_abline(intercept = 0, slope = 1, colour = dlinecol, linetype = 1,
                size = dlinethick) +
    coord_equal() +
    theme_bw() +
    theme(plot.title = element_text(size = fsize, colour = 'black', hjust = 0.5),
          axis.text.y = element_text(size = fsize, colour = 'black'),
          axis.text.x = element_text(size = fsize, colour = 'black', 
                                     angle = 90, vjust = 0.5, hjust=1),
          legend.title=element_blank(),
          legend.text=element_text(size=fsize),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x = element_text(size = fsize, colour = 'black'),
          axis.title.y = element_text(size = fsize, colour = 'black')) +
    xlab('False positive rate') +
    ylab('True positive rate') +
    ggtitle(title) +
    scale_color_manual(values=cols,name = 'Group', labels = llabels)
  return(g)
}

proc <- function(rocm,title='',cols=NULL,
                 rlinethick=1.5,fsize=15,dlinecol='grey',
                 dlinethick=0.75,llabels = llabels,G1=G2,
                 G2=G2){
  ## compute baseline
  P <- sum(rocm$obs==G2)
  PN <- length(rocm$obs)
  bl <- P/PN
  ##
  g <- ggplot(rocm, aes(x = SENS, y = PREC, color = Group)) + 
    geom_line(size = rlinethick) +
    geom_abline(intercept = bl, slope = 0, colour = dlinecol, linetype = 1,
                size = dlinethick) +
    coord_equal() +
    theme_bw() +
    theme(plot.title = element_text(size = fsize, colour = 'black', hjust = 0.5),
          axis.text.y = element_text(size = fsize, colour = 'black'),
          axis.text.x = element_text(size = fsize, colour = 'black', 
                                     angle = 90, vjust = 0.5, hjust=1),
          legend.title=element_blank(),
          legend.text=element_text(size=fsize),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x = element_text(size = fsize, colour = 'black'),
          axis.title.y = element_text(size = fsize, colour = 'black')) +
    xlab('Recall(sensitivity)') +    
    ylab('Precision') +
    ggtitle(title) +
    scale_color_manual(values=cols,name = 'Group', labels = llabels) +
    scale_y_continuous(limits = c(0,1))
  return(g)
}

prg <- function(rocm,title='',cols=NULL,
                 rlinethick=1.5,fsize=15,
                 llabels = llabels,G1=G2,
                 G2=G2){
  ## negatives are ignored
  rocm$RG[rocm$RG<0] <- 0
  rocm$PG[rocm$PG<0] <- 0
  ##
  g <- ggplot(rocm, aes(x = RG, y = PG, color = Group)) + 
    geom_line(size = rlinethick) +
    coord_equal() +
    theme_bw() +
    theme(plot.title = element_text(size = fsize, colour = 'black', hjust = 0.5),
          axis.text.y = element_text(size = fsize, colour = 'black'),
          axis.text.x = element_text(size = fsize, colour = 'black', 
                                     angle = 90, vjust = 0.5, hjust=1),
          legend.title=element_blank(),
          legend.text=element_text(size=fsize),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x = element_text(size = fsize, colour = 'black'),
          axis.title.y = element_text(size = fsize, colour = 'black')) +
    xlab('Recall gain') +    
    ylab('Precision gain') +
    ggtitle(title) +
    scale_color_manual(values=cols,name = 'Group', labels = llabels) +
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(limits = c(0,1))
  return(g)
}

cc <- function(rocm,title='',cols=NULL,G1=G1,G2=G2,
               rlinethick=1.5,fsize=15,dlinecol='grey',
               dlinethick=0.75,llabels = llabels,
               bins=bins){
  
  ## code to bin pred probabilities and compare with mean
  p2 <- NULL
  for (group in unique(rocm$Group)){
    p <- subset(rocm,rocm$Group==group)
    # process data
    p$obs <- as.character(p$obs)
    p$obs[p$obs!=G2] <- '0'
    p$obs[p$obs==G2] <- '1'
    p <- data.frame(pred=p[[G2]],obs=as.numeric(p$obs))
    # make bins
    p$bin <- cut(p$pred, bins)
    # get bin means
    predmeans <- data.frame(tapply(p$pred, cut(p$pred, bins), mean))
    colnames(predmeans)[1] <- 'predmean'
    realmeans <- data.frame(tapply(p$obs, cut(p$pred, bins), mean))
    colnames(realmeans)[1] <- 'realmean'
    predmeans$bin <- row.names(predmeans)
    realmeans$bin <- row.names(realmeans)
    d1 <- merge(p,predmeans,by='bin')
    d2 <- merge(d1,realmeans,by='bin')
    d2 <- d2[,c('bin','predmean','realmean')]
    deduped.data <- unique( d2[ , 1:3 ] )
    deduped.data$Group <- group
    ##
    p2 <- rbind(p2,deduped.data)
  }
  
  ## plot code
  cc <- ggplot(p2, aes(x = predmean, y = realmean, color = Group)) + 
    geom_abline(intercept = 0, slope = 1,colour = dlinecol, linetype = 1,
                size = dlinethick) +
    geom_line(size = rlinethick) +
    geom_point() +
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(limits = c(0,1)) +
    xlab('Predicted probability') +    
    ylab('True probability in each bin') +
    theme_bw() +
    theme(plot.title = element_text(size = fsize, colour = 'black', hjust = 0.5),
          axis.text.y = element_text(size = fsize, colour = 'black'),
          axis.text.x = element_text(size = fsize, colour = 'black', 
                                     angle = 90, vjust = 0.5, hjust=1),
          legend.title=element_blank(),
          legend.text=element_text(size=fsize),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x = element_text(size = fsize, colour = 'black'),
          axis.title.y = element_text(size = fsize, colour = 'black')) +
    ggtitle(title) +
    scale_color_manual(values=cols,name = 'Group', labels = llabels)
  return(cc)
}

ciauc <- function(auc,N1,N2,ci=95){
  # Hanley, James A., and Barbara J. McNeil. "The meaning and use of the area under a 
  # receiver operating characteristic (ROC) curve." Radiology 143.1 (1982): 29-36.
  # https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/PASS/
  # Confidence_Intervals_for_the_Area_Under_an_ROC_Curve.pdf
  z <- qnorm(1-((1-(ci/100))/2))
  Q1 <- auc/(2-auc)
  Q2 <- (2*auc^2)/(1+auc)
  s1 <- (auc*(1-auc)+(N1-1)*(Q1-auc^2)+(N2-1)*(Q2-auc^2))/(N1*N2)
  se_auc <- sqrt(s1)
  ui <- auc+(z*se_auc)
  li <- auc-(z*se_auc)
  ciauc <- c(round(li,2),round(ui,2))
  ciauc <- paste(ciauc,collapse='-')
  return(ciauc)
}

wci <- function(p,n,ci=95){
  # n = total in fraction e.g. TP+FN or TP+FP
  # p = fraction value in sample, i.e. p^
  # z = z
  # wilson score CI
  # https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/
  # pdf/Procedures/PASS/Confidence_Intervals_for_One-Sample_Sensitivity_and_Specificity.pdf
  z <- qnorm(1-((1-(ci/100))/2))
  num <- (2*n*p+z^2)+z*sqrt((z^2)+4*n*p*(1-p))
  denom <- 2*(n+z^2)
  ui <- num/denom
  num <- (2*n*p+z^2)-z*sqrt((z^2)+4*n*p*(1-p))
  denom <- 2*(n+z^2)
  li <- num/denom
  ciw <- c(round(li,2),round(ui,2))
  ciw <- paste(ciw,collapse='-')
  return(ciw)
}

#' brier_score: A Brier score function
#'
#' Calculates the Brier score to evaluate probabilities. A data frame of probabilities and ground truth labels must
#' be passed in to evaluate. Raw probability data must be column1: prob G1, column2: prob G2,
#' column3: obs labels, column4: Group (optional). Zero is optimal and more positive is less.
#'
#' @param preds Data frame: Data frame of probabilities and ground truth labels.
#' @param positive Character vector: The name of the positive group, must equal a column name consisting of probabilities.

#' @return
#' Brier score
#' @export
#'
#' @examples
#' r2 <- brier_score(preds)
brier_score <- function(preds,positive=colnames(preds)[2]){
  # Zero is optimal and more positive is less
  bs <- mean((preds[[positive]]-as.integer(preds$obs == positive))^2)
  return(bs)
}

#' LL: Log-likelihood function
#'
#' Calculates the Log-likelihood to evaluate probabilities. A data frame of probabilities and ground truth labels must
#' be passed in to evaluate. Raw probability data must be column1: prob G1, column2: prob G2,
#' column3: obs labels, column4: Group (optional). Zero is optimal and more negative is less.
#'
#' @param preds Data frame: Data frame of probabilities and ground truth labels.
#' @param positive Character vector: The name of the positive group, must equal a column name consisting of probabilities.

#' @return
#' Log-likelihood
#' @export
#'
#' @examples
#' r1 <- LL(preds)
LL <- function(preds,positive=colnames(preds)[2]){
  # Zero is optimal and more negative is less
  y <- as.integer(preds$obs == positive) # ground truth
  p <- preds[[positive]]
  L <- sum(log(p*y+(1-y)*(1-p)))
  return(L)
}
