
# David L Gibbs
# dgibbs@systemsbiology.org

# this script will form the backend of the shiny app.


# this function computes scores given some expression data.
newScores <- function(fileinfo, logflag, cores) {
  
  print(fileinfo)
  if(is.null(fileinfo)) {
    print("HERE")
    fileinfo <- list(name='ivy20.csv',  size=1, type='text/csv', datapath='data/ivy20.csv')
  }
  print(fileinfo)
  
  #source('functions/signature_mclust_ensemble.R')
  #source('functions/ImmuneSigs68_function.R')
  load('data/comparative_immuneSigs_geneLists4.rda')
  
  #incProgress()
  
  if (fileinfo$type == 'text/csv') {
    s1 <- ','
  } else if (fileinfo$type == 'text/csv') {
    s1 <- '\t'
  } else {
    s1 <- '\t'    
  }
  
  newdata <- read.table(fileinfo$datapath, sep=s1, header=T, stringsAsFactors = F)
  
  #incProgress()
  
  zscore.cols2<-function(x){
    return((apply(x, 2, function(x) (x - median(na.omit(x)))/sd(na.omit(x)))))
  }
  
  # 1 Recomputed non-Z-scored scores from the EBPP matrix.
  load("data/ebpp_scores.rda")
  
  #incProgress()
  
  # 2 we get some new data in.. require:
  #    it is RPKM  
  #    and   log2 transformed 
  #    and   gene symbols as row names
  #    and   median centered
  dat <- newdata[,-1]  # needs row names as symbols
  rownames(dat) <- newdata[,1]
  
  # just in case we need a log transform
  if (logflag) {
    datlog2 <- log2(dat+1)
  } else {
    datlog2 <- dat
  }
  
  ### median scaled for each gene
  #incProgress()
  datmeds <- apply(datlog2, 1, median, na.rm=T)  
  datscaled <- sweep(datlog2,1,datmeds,'-')
  datScores <- ImmuneSigs_function(datscaled, sigs1_2_eg2,sigs12_weighted_means,sigs12_module_weights,sigs1_2_names2,sigs1_2_type2)
  
  #incProgress()
  
  # then batch correction between scores...
  #incProgress()
  df <- as.matrix(cbind(datScores[rownames(ebppScores),], ebppScores))
  batch <- c(rep(1,ncol(datScores)), rep(2,ncol(ebppScores)))
  modcombat = model.matrix(~1, data=as.data.frame(t(df)))
  combat_edata = ComBat(dat=df, batch=batch, mod=modcombat, 
                        par.prior=TRUE, prior.plots=FALSE, ref.batch = 2)
  
  # and we subset the 5 scores used in clustering
  idx <- c("LIexpression_score", "CSF1_response", "TGFB_score_21050467", "Module3_IFN_score", "CHANG_CORE_SERUM_RESPONSE_UP")
  scores <- t(combat_edata[idx,])
  zscores <- zscore.cols2(scores)
  
  # load the clustering model trained on all pancan data.
  #incProgress()
  load("data/wolf_set_slim1.rda")
  calls <- consensusEnsemble(mods2, zscores, cores)
  maxcalls <- apply(calls$.Data, 1, function(a) which(a == max(a))[1])
  
  # and get the reported scores from the manuscript
  wolf <- read.table("data/five_signature_mclust_ensemble_results.tsv.gz", sep='\t', header=T, stringsAsFactors = F)
  wolfscrs <- wolf[,c(5:9)]
  wolfNames <- str_replace_all(wolf$AliquotBarcode, '\\.', '-')
  
  # Then we make sure the pancan cluster labels have not changed *much*  *how much is OK?*
  idx <- match(table=rownames(scores), x = wolfNames)
  t1 <- table(New=maxcalls[idx], Wolf=wolf$ClusterModel1)
  t2 <- t1
  for (i in 1:6) {
    kdx <- which(t1[,i] == max(t1[,i]))
    t2[i,] <- round(t1[kdx,]/sum(t1[kdx,]), digits = 3) 
  }
  rownames(t2) <- c('C1', 'C2', 'C3', 'C4', 'C5', 'C6')
  
  #incProgress()
  
  jdx <- match(table=rownames(scores), x=colnames(dat))
  pcalls <- calls$.Data[jdx,]
  rownames(pcalls) <- colnames(dat)
  pcalls <- cbind(pcalls, data.frame(Call=maxcalls[jdx]))
  pcalls <- cbind(pcalls, zscores[jdx,])
  
  newMaxCalls <- maxcalls[jdx]
  
  print(newMaxCalls)
  
  return(list(MaxCalls=newMaxCalls, Table=t2, ProbCalls=pcalls))
  
}


