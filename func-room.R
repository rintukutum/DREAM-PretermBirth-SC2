ctrl.sptd <- function(x){
  x.ctrl <- x[idx.ctrl]
  x.sptd <- x[idx.sptd]
  df_ <- data.frame(
    class = rep(
      c('ctrl','sptd'),
      c(length(x.ctrl),
        length(x.sptd))),
    value = c(
      x.ctrl,
      x.sptd
    )
  )
  normal <- shapiro.test(df_$value)$p.value > 0.05
  if(normal){
    pval <- t.test(value~class,data=df_)$p.value
  }else{
    pval <- wilcox.test(value~class,data=df_)$p.value
  }
  return(pval)
}
ctrl.pprom <- function(x){
  x.ctrl <- x[idx.ctrl]
  x.pprom <- x[idx.pprom]
  df_ <- data.frame(
    class = rep(
      c('ctrl','pprom'),
      c(length(x.ctrl),
        length(x.pprom))),
    value = c(
      x.ctrl,
      x.pprom
    )
  )
  normal <- shapiro.test(df_$value)$p.value > 0.05
  if(normal){
    pval <- t.test(value~class,data=df_)$p.value
  }else{
    pval <- wilcox.test(value~class,data=df_)$p.value
  }
  return(pval)
}

performSVM <- function(tr.x,tr.y,tt.x,SEED=7860){
  # tr.x <- ctrl.PPROM.sig.2H.probes.data$tr.x[,1:100]
  # tr.y <- ctrl.PPROM.sig.2H.probes.data$tr.y
  # tt.x <- ctrl.PPROM.sig.2H.probes.data$tt.x[,1:100]
  library(e1071)
  library(caret)
  library(foreach)
  set.seed(SEED)
  cv.5 <- createFolds(tr.y,k = 5)
  out <- list()
  for(i in 1:length(cv.5)){
    idx.tr <- unlist(cv.5[-i])
    idx.tt <- unlist(cv.5[i])
    mini.tr.x <- tr.x[idx.tr,]
    mini.tr.y <- tr.y[idx.tr]
    
    mini.tt.x <- tr.x[idx.tt,]
    mini.tt.y <- tr.y[idx.tt]
    
    #--------------------
    # MODELS
    mod.linear <- svm(
      x = mini.tr.x,
      y= mini.tr.y,
      kernel = 'linear',
      probability = TRUE
    )
    pred.linear <- predict(
      mod.linear,
      mini.tt.x
    )
    
    mod.radial <- svm(
      x = mini.tr.x,
      y= mini.tr.y,
      kernel = 'radial',
      probability = TRUE
    )
    pred.radial <- predict(
      mod.radial,
      mini.tt.x
    )
    
    mod.sigmoid <- svm(
      x = mini.tr.x,
      y= mini.tr.y,
      kernel = 'sigmoid',
      probability = TRUE
    )
    pred.sigmoid <- predict(
      mod.sigmoid,
      mini.tt.x
    )
    tmpOUT.r <- getCVperf(
      pred = pred.radial,
      orig = mini.tt.y,
      mod.method = 'SVM.radial'
    )
    tmpOUT.s <- getCVperf(
      pred = pred.sigmoid,
      orig = mini.tt.y,
      mod.method = 'SVM.sigmoid'
    )
    tmpOUT.l <- getCVperf(
      pred = pred.linear,
      orig = mini.tt.y,
      mod.method = 'SVM.linear'
    )
    perf.svm <- list(
      tmpOUT.r,
      tmpOUT.s,
      tmpOUT.l
    )
    perf.svm.df <- plyr::ldply(perf.svm)
    #---------
    # PREDICT TT
    tt.linear <- predict(mod.linear,tt.x,probability = TRUE)
    tt.linear <- data.frame(round(attr(tt.linear,'probabilities'),4))
    tt.linear$SampleID <- rownames(tt.linear)
    tt.sigmoid <- predict(mod.sigmoid,tt.x,probability = TRUE)
    tt.sigmoid <- data.frame(round(attr(tt.sigmoid,'probabilities'),4))
    tt.sigmoid$SampleID <- rownames(tt.sigmoid)
    tt.radial <- predict(mod.radial,tt.x,probability = TRUE)
    tt.radial <- data.frame(round(attr(tt.radial,'probabilities'),4))
    tt.radial$SampleID <- rownames(tt.radial)
    tt.out <- list(
      'linear' = tt.linear,
      'sigmoid' = tt.sigmoid,
      'radial' = tt.radial
    )
    OUT <- list(
      perf = perf.svm.df,
      tt.prediction = plyr::ldply(tt.out)
    )
    out[[i]] <- OUT
  }
  names(out) <- paste0('CV',1:length(cv.5))
  return(out)
}
getCVperf <- function(pred,
                      orig,
                      mod.method){
  xout <- caret::confusionMatrix(
    data = pred,
    reference=orig
  )
  df_ <- data.frame(xout$byClass[c('Sensitivity','Specificity')])
  colnames(df_)[1] <- 'val'
  df_$property <- rownames(df_)
  df_$method <- mod.method
  return(df_)
}
extractMODpred <- function(x,method){
  x <- x$tt.prediction
  idx <- x$.id %in% method
  return(x[idx,])
}
