#Copyright 2018 David L Gibbs, Institute for Systems Biology
#
#Permission is hereby granted, free of charge, to any person obtaining a 
#copy of this software and associated documentation files (the "Software"), 
#to deal in the Software without restriction, including without limitation the 
#rights to use, copy, modify, merge, publish, distribute, sublicense, and/or 
#sell copies of the Software, and to permit persons to whom the Software is 
#furnished to do so, subject to the following conditions:
#
#The above copyright notice and this permission notice shall be included 
#in all copies or substantial portions of the Software.
#
#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
#INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
#PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE 
#LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, 
#TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
#OR OTHER DEALINGS IN THE SOFTWARE.



# if a feature has 0 MAD then ignore it.
mcentr <- function(x) {
  meds <- median(x, na.rm=T)
  mads <- mad(x, na.rm=T)
  if (mads == 0) {
    rep(0, length(x))
  } else {
    (x - meds) / mads
  }
}


modelEnsemble <- function(dat, mods=10, klus=6, sampSize=0.5, init=0.25, cores=2) {
  # dat: expression matrix, signature scores in columns 
  # mods: number of models
  # klus: number of clusters
  # sampSize: percent of samples to sample
  # init: portion of samples to use for init 
  # cores: number of CPUs to use
  #
  # for each rep, make a list of splits
  s1 <- lapply(1:mods, function(a) sample(1:nrow(dat), size=nrow(dat)*sampSize, replace=F))
  # cluster each split
  modList <- mclapply(s1, function(a) {
    Mclust(dat[a,], G=klus, initialization=list(subset=sample(1:length(a), size=(init * length(a)))))
  }, mc.cores=cores)
  modList
}

ensemblePredict <- function(modList, dat, mode="list", cores, ensemblesize) {
  # returns a matrix of cluster assignments
  # dat: expression matrix, signature scores in columns 
  # modList: list of mclust models
  # mode: how to bind the results ... use 'list' or 'matrix'
  #pred <- lapply(modList, function(a) predict(a, dat)$classification)
  
  require(mclust)  ### Error when calling library in the global function for some reason!
  
  idx <- sample(x = 1:length(modList), size = ensemblesize, replace = F)
  
  pred <- mclapply(modList[idx], function(a) mclust::predict.Mclust(a, dat)$classification, mc.cores=cores)

  # then unload mclust
  detach(package:mclust, unload=TRUE)
  
  if (mode == "matrix") {
    return(do.call("cbind", pred))
  } else {
    return(pred)
  }
}

consensusEnsemble <- function(modList, dat, cores=2, ensemblesize) {
  # make consensus calls
  # dat: expression matrix, signature scores in columns 
  # modList: list of mclust models
  preds <- ensemblePredict(modList,dat,"list", cores, ensemblesize)
  partitions <- lapply(preds, function(a) as.cl_partition(a))
  clpart <- cl_ensemble(list=partitions)
  consensus <- cl_consensus(clpart, method="GV1")
  consensus
}


pred1 <- function(dat, klus, init) {
  # used in determining K, 'klus', the number of clusters
  # dat: expression matrix, signature scores in columns 
  # klus: number of clusters
  # init: portion of samples to use for init
  kperf <- c()
  try({
    s1 <- sample(1:nrow(dat), size=nrow(dat)/2, replace=F)
    s2 <- (1:nrow(dat))[! (1:nrow(dat)) %in% s1]
    
    res1 <- Mclust(dat[s1,], G=klus, initialization=list(subset=sample(1:length(s1), size=(init * length(s1))))  )
    res2 <- Mclust(dat[s2,], G=klus, initialization=list(subset=sample(1:length(s2), size=(init * length(s2))))  )
    
    # train classifier
    pre2a <- predict(res1, dat[s2,])
    
    # then, for each pair of samples in a cluster
    #   is that pair in the same predicted cluster?
    df <- data.frame(Pred=pre2a$classification, Res2=res2$classification)
    
    kperf <- rep(0, klus)
    for (ci in 1:klus) {
      samps <- rownames(df)[df$Res2 == ci]   # get samples from this cluster
      cas   <- as.numeric(df[samps, "Pred"]) # get clusters predicted on these samples
      tcas  <- table(cas)                     # count labels
      casName <- as.numeric(names(tcas)[which(tcas == max(tcas))]) # most common label
      kperf[ci] <- sum(cas == casName) / length(cas) # percent in majority label
    }
  })
  kperf
}


predStrength <- function(dat, klus, init=0.1, cores=2, reps) {
  # predictive strength
  # With this number of clusters, is it reproducible and predicable?
  # 
  # dat: expression matrix, signature scores in columns 
  # klus: number of clusters
  # init: portion of samples for init
  # cores: number of CPUs to use
  # reps: number of repititions
  repList <- mclapply(1:reps, function(a) {
    pred1(dat, klus, init)
  }, mc.cores=cores)
  
  repList <- lapply(repList, function(a) sort(a))
  do.call("rbind", repList)
}


determineK <- function(dat, init=0.25, cores=2, reps=10) {
  # for each rep
  # make a list of splits
  # dat: expression matrix, signature scores in columns 
  # init: portion of samples to use for init
  # cores, reps, see above.
  s1 <- lapply(1:reps, function(a) sample(1:nrow(dat), size=nrow(dat)/2, replace=F))
  # cluster each split
  modList <- mclapply(s1, function(a) {
    Mclust(dat[a,], initialization=list(subset=sample(1:length(a), size=(init * length(a)))))
  }, mc.cores=cores)
  
  ks <- lapply(modList, function(a) a$G)
  unlist(ks)
}


#####################################################

#wolf <- read.table("data/five_signature_mclust_ensemble_results.tsv", sep='\t', header=T, stringsAsFactors = F)
#dat <- wolf[,c(5:9)]
#wolfNames <- str_replace_all(wolf$AliquotBarcode, '\\.', '-')

#check1 <- modelEnsemble(dat, mods=256, klus=6, sampSize=0.5, cores=6)
#checkP <- consensusEnsemble(check1, dat)
#checkC <- apply(checkP$.Data, 1, function(a) which(a == max(a))[1])

#table(checkC, wolf$ClusterModel1)


