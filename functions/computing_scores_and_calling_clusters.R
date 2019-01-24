
# David L Gibbs
# dgibbs@systemsbiology.org

# this script will form the backend of the shiny app.


# this function computes scores given some expression data.
newScores <- function(fileinfo, logflag, cores, ensemblesize, combatflag) {
  
  print(fileinfo)
  if(is.null(fileinfo)) {
    print("HERE")
    fileinfo <- list(name='ivy20.csv',  size=1, type='text/csv', datapath='data/ivy20.csv')
  }
  print(fileinfo)
  
  #source('functions/signature_mclust_ensemble.R')
  #source('functions/ImmuneSigs68_function.R')
  load('data/comparative_immuneSigs_geneLists4.rda')
  
  if (fileinfo$type == 'text/csv') {
    s1 <- ','
  } else if (fileinfo$type == 'text/csv') {
    s1 <- '\t'
  } else {
    s1 <- '\t'    
  }
  
  newdata <- read.table(fileinfo$datapath, sep=s1, header=T, stringsAsFactors = F)
  # write.table(tcgaSubset, file='data/ebppSubset.tsv', sep='\t', quote=F)
  # newdata <- newData
  
  zscore.cols2<-function(x){
    return((apply(x, 2, function(x) (x - median(na.omit(x)))/sd(na.omit(x)))))
  }
  
  # 1. the EBPP expression data subset
  tcgaSubset <- read.table('data/ebppSubset.tsv.bz2', header = T, sep = '\t', stringsAsFactors = F)
  reportedScores <- read.table('data/five_signature_mclust_ensemble_results.tsv.gz', sep='\t', header=T, stringsAsFactors = F)
  rownames(reportedScores) <- reportedScores$AliquotBarcode
  
  # done already
  #tcgaSubset <- tcgaSubset[,colnames(tcgaSubset) %in% reportedScores$AliquotBarcode]
  tcgaSubset <- log2(tcgaSubset + 1)
  
  # 2 we get some new data in.. require:
  #    it is RPKM  
  #    and   log2 transformed 
  #    and   gene symbols as row names
  #    and   median centered
  didx <- !duplicated(as.character(newdata[,1]))
  dat <- newdata[didx,]
  rownames(dat) <- dat[,1]
  dat <- dat[,-1]  # needs row names as symbols
  
  # just in case we need a log transform
  if (logflag) {
    datlog2 <- log2(dat+1)
  } else {
    datlog2 <- dat
  }
  
  
  ### joining data sets ###
  sharedGenes  <- intersect(rownames(tcgaSubset), rownames(dat))

  # first median scale each data set
  newDatSub    <- datlog2[sharedGenes,]
  newDatSubMeds<- apply(newDatSub, 1, median, na.rm=T)  
  newDatSub    <- sweep(newDatSub,1,newDatSubMeds,'-')
  
  tcgaSubsetSub <- tcgaSubset[sharedGenes,]
  tcgaSubsetSubMeds <- apply(tcgaSubsetSub, 1, median, na.rm=T)  
  tcgaSubsetSub <- sweep(tcgaSubsetSub,1,tcgaSubsetSubMeds,'-')
  
  # then join them at the genes
  joinDat      <- cbind(newDatSub, tcgaSubsetSub)
  
  if (combatflag) {
    # then batch correction between scores...
    batch <- c(rep(1,ncol(newDatSub)), rep(2,ncol(tcgaSubsetSub)))
    modcombat = model.matrix(~1, data=as.data.frame(t(joinDat)))
    combat_edata = ComBat(dat=joinDat, batch=batch, mod=modcombat, 
                          par.prior=TRUE, prior.plots=FALSE, ref.batch = 2)
  } else {
    combat_edata = joinDat    
  }
  
  ### compute scores.
  datScores <- ImmuneSigs_function(combat_edata, sigs1_2_eg2,sigs12_weighted_means,
                                   sigs12_module_weights,sigs1_2_names2,sigs1_2_type2,
                                   cores)

  
  # and we subset the 5 scores used in clustering
  idx <- c("LIexpression_score", "CSF1_response", "TGFB_score_21050467", "Module3_IFN_score", "CHANG_CORE_SERUM_RESPONSE_UP")
  scores <- t(datScores[idx,])
  zscores <- zscore.cols2(scores)

  # load the clustering model trained on all pancan data.
  #incProgress()
  load("data/wolf_set_slim1.rda")
  
  # make cluster calls using the models.
  calls <- consensusEnsemble(mods2, zscores, cores, ensemblesize)
  
  # get the top scoring cluster for each sample
  maxcalls <- apply(calls$.Data, 1, function(a) which(a == max(a))[1])
  names(maxcalls) <- rownames(scores)
  
  # then we'll look at the new vs. old cluster calls for TCGA samples
  sharedIDs <- intersect(reportedScores$AliquotBarcode, rownames(scores))
  t1 <-table(Reported=as.numeric(reportedScores[sharedIDs, 'ClusterModel1']),
             NewCalls=as.numeric(maxcalls[sharedIDs]))
  
  # then we can align the new calls to old calls.
  reported <- 1:6
  optcalls <- 1:6
  otherway <- 1:6
  for (i in reported) {
    
    # for subtype i, where did most of the samples end up?
    j <- which(as.numeric(t1[i,]) == max(as.numeric(t1[i,])))
    # rename maxcall j <- i
    optcalls[i] <- j
    otherway[j] <- i
  }
  
  print(optcalls)
  print(otherway)
  
  # these are the re-mapped calls
  alignedCalls <- sapply(maxcalls, function(a) which(a == optcalls)[1])

  # make sure it works
  t2 <-table(Reported=as.numeric(reportedScores[sharedIDs, 'ClusterModel1']),
             NewCalls=as.numeric(alignedCalls[sharedIDs]))

  # assemble the results
  jdx <- match(table=rownames(scores), x=colnames(dat))  # index to new data scores
  pcalls <- calls$.Data[jdx,]                            # get that table
  rownames(pcalls) <- colnames(dat)                      # name it from the new data
  pcalls <- pcalls[,optcalls]
  
  pcalls <- cbind(pcalls, data.frame(Call=alignedCalls[jdx]))  # bring in the aligned calls
  pcalls <- cbind(pcalls, zscores[jdx,])                       # and the scores
    
  return(list(AlignedCalls=alignedCalls[jdx], Table=t2, ProbCalls=pcalls))
  
  
  ###########################################################################################
  ### OLD CODE   
  #maxcalls <- apply(calls$.Data, 1, function(a) which(a == max(a))[1])
  
  # and get the reported scores from the manuscript
  #wolf <- read.table("data/five_signature_mclust_ensemble_results.tsv.gz", sep='\t', header=T, stringsAsFactors = F)
  #wolfscrs <- wolf[,c(5:9)]
  #wolfNames <- str_replace_all(wolf$AliquotBarcode, '\\.', '-')
  
  # Then we make sure the pancan cluster labels have not changed *much*  *how much is OK?*
  #idx <- match(table=rownames(scores), x = wolfNames)
  #t1 <- table(New=maxcalls[idx], Wolf=wolf$ClusterModel1)
  #t2 <- t1
  #for (i in 1:6) {
  #  kdx <- which(t1[,i] == max(t1[,i]))
  #  t2[i,] <- round(t1[kdx,]/sum(t1[kdx,]), digits = 3) 
  #}
  #rownames(t2) <- c('C1', 'C2', 'C3', 'C4', 'C5', 'C6')
  
  #jdx <- match(table=rownames(scores), x=colnames(dat))
  #pcalls <- calls$.Data[jdx,]
  #rownames(pcalls) <- colnames(dat)
  #pcalls <- cbind(pcalls, data.frame(Call=maxcalls[jdx]))
  #pcalls <- cbind(pcalls, zscores[jdx,])
  
  #newMaxCalls <- maxcalls[jdx]
  
  #print(newMaxCalls)
  
  #return(list(MaxCalls=newMaxCalls, Table=t2, ProbCalls=pcalls))
  
}


