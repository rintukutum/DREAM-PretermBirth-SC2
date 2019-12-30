rm(list=ls())
load('./data/HTA20_RMA_probeset.RData')
metadata <- read.csv(
  './data/anoSC2_v20_nokey.csv',
  stringsAsFactors = FALSE
)
#' CONTROL vs sPTD
idx.tr <- metadata$Group %in% c('Control','sPTD')
idx.test <- metadata$Train == 0
tr.class <- metadata$Group[idx.tr]
tr.sample <- metadata$SampleID[idx.tr]
tt.sample <- metadata$SampleID[idx.test]

names(tr.class) <- tr.sample
tr.y <- tr.class[
  intersect(colnames(eset_HTA20_probeset),
            names(tr.class))]
load('./data/ctrl.sptd.pval.RData')
probes.sig.2H <- names(sort(ctrl.sptd.pval)[1:200])

tr.x <- t(eset_HTA20_probeset[probes.sig.2H,names(tr.y)])
tt.x <- t(eset_HTA20_probeset[probes.sig.2H,tt.sample])
ctrl.sPTD.sig.2H.probes.data <- list(
  tr.y = as.factor(tr.y),
  tr.x = tr.x,
  tt.x = tt.x
)
save(ctrl.sPTD.sig.2H.probes.data,
     file = './data/ctrl.sPTD.sig.2H.probes.data.RData')
rm(list=ls())
load('./data/HTA20_RMA_probeset.RData')
metadata <- read.csv(
  './data/anoSC2_v20_nokey.csv',
  stringsAsFactors = FALSE
)
#' CONTROL vs PPROM
idx.tr <- metadata$Group %in% c('Control','PPROM')
idx.test <- metadata$Train == 0
tr.class <- metadata$Group[idx.tr]
tr.sample <- metadata$SampleID[idx.tr]
tt.sample <- metadata$SampleID[idx.test]

names(tr.class) <- tr.sample
tr.y <- tr.class[
  intersect(colnames(eset_HTA20_probeset),
  names(tr.class))]
load('./data/ctrl.pprom.pval.RData')
probes.sig.2H <- names(sort(ctrl.pprom.pval)[1:200])

tr.x <- t(eset_HTA20_probeset[probes.sig.2H,
                              names(tr.y)])
tt.x <- t(eset_HTA20_probeset[probes.sig.2H,
                              tt.sample])
ctrl.PPROM.sig.2H.probes.data <- list(
  tr.y = as.factor(tr.y),
  tr.x = tr.x,
  tt.x = tt.x
)
save(ctrl.PPROM.sig.2H.probes.data,
     file = './data/ctrl.PPROM.sig.2H.probes.data.RData')