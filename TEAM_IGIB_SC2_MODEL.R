rm(list=ls())
#---------------------------------------------------
#---------------------------------------------------
#---------------------------------------------------
cat('#----------------------------\n')
cat('Load HTA20_RMA_probeset.RData\n')
cat('Loading "./data/HTA20_RMA_probeset.RData"\n')
load('./data/HTA20_RMA_probeset.RData')
cat('#----------------------------\n')
cat('Load metadata\n')
cat('Loading "./data/anoSC2_v20_nokey.csv"\n')
metadata <- read.csv(
  './data/anoSC2_v20_nokey.csv',
  stringsAsFactors = FALSE
)
cat('#----------------------------\n')
train.sample <- metadata$SampleID[metadata$Train == 1]
test.sample <- metadata$SampleID[metadata$Train == 0]

sPTD.sample <- metadata$SampleID[metadata$Group == 'sPTD']
ctrl.sample <- metadata$SampleID[metadata$Group == 'Control']
pprom.sample <- metadata$SampleID[metadata$Group == 'PPROM']

idx.ctrl <- colnames(eset_HTA20_probeset) %in% ctrl.sample
idx.sptd <- colnames(eset_HTA20_probeset) %in% sPTD.sample
idx.pprom <- colnames(eset_HTA20_probeset) %in% pprom.sample

source('./func-room.R')
cat('DE | Control vs sPTD comparing (est. time. 40 mins)\n')
ctrl.sptd.pval  <- apply(
  eset_HTA20_probeset,
  1,
  function(x){
    ctrl.sptd(x)
  }
)
cat('DE | Control vs sPTD done!\n')
cat('DE | Control vs PPROM (est. time. 40 mins)\n')
ctrl.pprom.pval  <- apply(
  eset_HTA20_probeset,
  1,
  function(x){
    ctrl.pprom(x)
  }
)
cat('DE | Control vs PPROM done\n')
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
#---------------------------------------------------
#---------------------------------------------------
#---------------------------------------------------
cat("#-------------------------------\n")
cat("Build model\n")
cat("#-------------------------------\n")
rm(list = ls())
load('./data/ctrl.sPTD.sig.2H.probes.data.RData')
load('./data/ctrl.PPROM.sig.2H.probes.data.RData')
source('func-room.R')

mod.data <-ctrl.PPROM.sig.2H.probes.data
svmMODs.PPROM <- performSVM(
  tr.x = mod.data$tr.x[,1:100],
  tr.y = mod.data$tr.y,
  tt.x = mod.data$tt.x[,1:100]
)

mod.data <-ctrl.sPTD.sig.2H.probes.data
svmMODs.sPTD <- performSVM(
  tr.x = mod.data$tr.x[,1:100],
  tr.y = mod.data$tr.y,
  tt.x = mod.data$tt.x[,1:100]
)

rad.sPTD <- lapply(
  svmMODs.sPTD,
  function(x){
    extractMODpred(x = x,method = 'radial')
  }
)
rad.sPTD.df <- plyr::ddply(
  plyr::ldply(rad.sPTD),
  'SampleID',
  function(x){
    out <- apply(x[,2:3],2,mean)
    return(out)
  })

rad.PPROM <- lapply(
  svmMODs.PPROM,
  function(x){
    extractMODpred(x = x,method = 'radial')
  }
)
rad.PPROM.df <- plyr::ddply(
  plyr::ldply(rad.PPROM),
  'SampleID',
  function(x){
    out <- apply(x[,2:3],2,mean)
    return(out)
  })

metadata <- read.csv(
  './data/anoSC2_v20_nokey.csv',
  stringsAsFactors = FALSE
)
indID <- metadata$IndividualID
names(indID) <- metadata$SampleID
rad.PPROM.df$IndividualID <- indID[rad.PPROM.df$SampleID]
rad.sPTD.df$IndividualID <- indID[rad.sPTD.df$SampleID]

rad.sub.SC2 <- data.frame(
  IndividualID = rad.PPROM.df$IndividualID,
  sPTD = rad.sPTD.df$sPTD,
  PPROM = rad.PPROM.df$PPROM
)
rownames(rad.sub.SC2) <- rad.sub.SC2$IndividualID
ind.L <- plyr::ddply(
  rad.sub.SC2,
  'IndividualID',
  function(x){
    apply(x[,2:3],2,max)
  }
)
write.csv(
  ind.L,
  file = './TEAM_IGIB_prediction_MAX.csv',
  row.names = FALSE,
  quote = FALSE
)

# ind.L.min <- plyr::ddply(
#   rad.sub.SC2,
#   'IndividualID',
#   function(x){
#     apply(x[,2:3],2,min)
#   }
# )
# 
# write.csv(
#   ind.L.min,
#   file = './TEAM_IGIB_prediction_MIN.csv',
#   row.names = FALSE,
#   quote = FALSE
# )
sink('TEAM_IGIB_info.txt')
sessionInfo()
sink()