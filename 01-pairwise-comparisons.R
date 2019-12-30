rm(list=ls())
load('./data/HTA20_RMA_probeset.RData')
metadata <- read.csv(
  './data/anoSC2_v20_nokey.csv',
  stringsAsFactors = FALSE
)

train.sample <- metadata$SampleID[metadata$Train == 1]
test.sample <- metadata$SampleID[metadata$Train == 0]

sPTD.sample <- metadata$SampleID[metadata$Group == 'sPTD']
ctrl.sample <- metadata$SampleID[metadata$Group == 'Control']
pprom.sample <- metadata$SampleID[metadata$Group == 'PPROM']

idx.ctrl <- colnames(eset_HTA20_probeset) %in% ctrl.sample
idx.sptd <- colnames(eset_HTA20_probeset) %in% sPTD.sample
idx.pprom <- colnames(eset_HTA20_probeset) %in% pprom.sample

source('./func-room.R')
ctrl.sptd.pval  <- apply(
  eset_HTA20_probeset,
  1,
  function(x){
    ctrl.sptd(x)
  }
)
save(ctrl.sptd.pval,
     file = './data/ctrl.sptd.pval.RData')

ctrl.pprom.pval  <- apply(
  eset_HTA20_probeset,
  1,
  function(x){
    ctrl.pprom(x)
  }
)
save(ctrl.pprom.pval,
     file = './data/ctrl.pprom.pval.RData')
