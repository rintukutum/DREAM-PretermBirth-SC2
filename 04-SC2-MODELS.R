rm(list=ls())
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
dir.create('./data/submission',showWarnings = FALSE)
write.csv(
  ind.L,
  file = './data/submission/RAD-SC2-SUB01.csv',
  row.names = FALSE,
  quote = FALSE
)

ind.L.min <- plyr::ddply(
  rad.sub.SC2,
  'IndividualID',
  function(x){
    apply(x[,2:3],2,min)
  }
)

dir.create('./data/submission',showWarnings = FALSE)
write.csv(
  ind.L.min,
  file = './data/submission/RAD-SC2-SUB01-MIN.csv',
  row.names = FALSE,
  quote = FALSE
)