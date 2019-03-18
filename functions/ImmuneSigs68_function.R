#Copyright 2018 Denise Wolf @ UCSF
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


##                                                                                                    ##
## This code was used to generate the signature scores that were then clustered into immune subtypes. ##
## The scores generated are the same ones found in five_signature_mclust_ensemble_results.tsv.gz      ##
##                                                                                                    ##

#######################REQUIRED FOR MODULES

#for modules
sample.scores<-function(signature.data, sig) {
  # sig must be a named with probesets as names
  # signature.data must be a matrix with samples as columns, probesets as rows with probesets named.
  signature.data <- signature.data[match(names(sig), rownames(signature.data)),]
  d <- apply(signature.data, 2, function(x) get.distances(sig, x))
  return(d[1,])
}
get.distances<-function(t1, sample.data) {
  centroid.distances <- as.matrix(dist(rbind(t1, d=sample.data, o=rep(0,length(t1)))))/sqrt(length(t1) - 1)
  c <- centroid.distances[1,3] # 1
  b <- centroid.distances[2,1]
  a <- centroid.distances[3,2]
  A <- acos((b^2 + c^2 - a^2)/(2*b*c))
  sigma <- sin(A)*b
  score <- 1 - (cos(A)*b)
  return(c(score=score, sigma=sigma))
}

###########Zscore
zscore.rows2<-function(x){
  return(t(apply(x, 1, function(x) (x - median(na.omit(x)))/sd(na.omit(x)))))
}


#tempImm<-ImmuneSigs_function(data2, sigs1_2_eg2,sigs12_weighted_means,sigs12_module_weights,sigs1_2_names2,sigs1_2_type2)


#############################################################
############ file= ISPY2plus_Sigs_function.R
###########  function=ImmuneSigs_function
###########Inputs: 1. data2=gene expression data with rows as genes, columns as samples, and using gene symbol as IDs
##                       data is assumed to be normalized and median centered, and collapsed to gene level.
##                 2. From the file "comparative_immuneSigs_geneLists4.rda", which you need to load, is all the data for the 
##                    the 68 immune signatures.  
##                         sigs1_2_egs=data structure with gene names, entrezIDs, and method of evaluation.
##                         sigs12_module_weights=gene weights for modules; 
##                         sigs12_module_weighted_means=gene weights for weighted signatures and centroids for correlation sigs
##                         sigs1_2_names2=signature name list
##                         sigs1_2_type2=evaluation method
##          Output= matrix of 68 immune signatures, zscore normalized with patient samples in columns
##          Auxilary functions called (bottom of this file): for modules: get.distances and sample.scores. Plus zscore2.
#############################################################################################################
###########################################################################################################

ImmuneSigs_function<-function(data2, 
                              sigs1_2_eg2,
                              sigs12_weighted_means,
                              sigs12_module_weights,
                              sigs1_2_names2,
                              sigs1_2_type2,
                              cores){
  
  library(parallel)
  ##gather together the gene names
  
  sigs1_2_geneIDs2<-as.character(na.omit(sigs1_2_eg2[[1]]$probe))
  for(i in 2:length(sigs1_2_eg2)){
    sigs1_2_geneIDs2<-c(sigs1_2_geneIDs2,as.character(na.omit(sigs1_2_eg2[[i]]$probe)))
  }
  sigs1_2_geneIDs2<-unique(sigs1_2_geneIDs2)   ## 2652 unique
  
  #######
  
  ##see how many genes are found in data 
  data_genes_sigs12b<-intersect(rownames(data2),sigs1_2_geneIDs2)  #only 2183 genes overlap using gene names (may be more if translate to probe)
  
  ##1#####################################
  ####first evaluate the signatures calculated as the mean expression of genes in the signature
  
  doSigs <- c(4,17,18,30,35)
  
  
  data_otherimmune2<-NA
  
  for(i in doSigs){
    
    if(sigs1_2_type2[i]=="mean"){
      vars<-intersect(data_genes_sigs12b,sigs1_2_eg2[[i]]$probe)
      if(length(vars)>0){
        dat<-data2[vars,]
        if(length(vars)==1){sig<-dat}
        if(length(vars)>1){sig<-apply(dat,2,mean,na.rm=T)}
        data_otherimmune2<-rbind(data_otherimmune2,sig)
        rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]<-sigs1_2_names2[i]
        #cat(paste(rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]),"\n")
      }
    }
  }
  
  
  
  print("DONE MEAN")
  
  data_otherimmune2<-data_otherimmune2[-1,]	#get rid of the first row of
  
  ##2#########################################
  ####next, evaluate the signatures calculated as the median expression level of genes in the signature
  
  
  for(i in doSigs){
    
    if(sigs1_2_type2[i]=="median"){
      vars<-intersect(data_genes_sigs12b,sigs1_2_eg2[[i]]$probe)
      if(length(vars)>0){
        dat<-data2[vars,]
        if(length(vars)==1){sig<-dat}
        if(length(vars)>1){sig<-apply(dat,2,median,na.rm=T)}
        data_otherimmune2<-rbind(data_otherimmune2,sig)
        rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]<-sigs1_2_names2[i]
        #cat(paste(rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]),"\n")
      }
    }
  }  
  
  print("DONE MEDIAN")
  
  ###3##########################################
  ####evaluate weighted_mean signatures
  #
  for(i in doSigs){
    
    if(sigs1_2_type2[i]=="weighted_mean"){
      vars<-intersect(data_genes_sigs12b,sigs1_2_eg2[[i]]$probe)
      if(length(vars)>0){
        dat<-data2[vars,]
        wt<-sigs12_weighted_means[[sigs1_2_names2[i]]][vars]
        dat<-wt*dat
        sig<-round(apply(dat,2,mean,na.rm=T),digits=5)
        data_otherimmune2<-rbind(data_otherimmune2,sig)
        rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]<-sigs1_2_names2[i]
        #cat(paste(rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]),"\n")
      }
    }
  }
  
  print("DONE WEIGHTED MEAN")
  
  ###4######################################
  ###evaluate special signatures
  
  
  #first, brca gene expression immune modules, and the proliferation module
  modules<-c("Module3_IFN_score") # ,"Module4_TcellBcell_score","Module5_TcellBcell_score","Module11_Prolif_score")
  for(i in 1:length(modules)){
    j=which(sigs1_2_names2==modules[i])
    vars<-intersect(data_genes_sigs12b,sigs1_2_eg2[[j]]$probe)
    if(length(vars)>0){
      dat<-data2[vars,]
      wt<-sigs12_module_weights[[i]][vars]
      sig<-sample.scores(dat,wt)
      data_otherimmune2<-rbind(data_otherimmune2,sig)
      rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]<-sigs1_2_names2[j]
      #cat(paste(rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]),"\n")
    }
    
  }
  
  print("DONE MODULES")
  
  ######now gene programs GP11_Immune_IFN and GP2_ImmuneTcellBcell_score  
  ##GP11 most correlated signatures are Minterferon_Cluster_21214954 and ZHANG_INTERFERON_RESPONSE, so take mean of two
  #i=which(sigs1_2_names2=="ZHANG_INTERFERON_RESPONSE")
  #vars<-intersect(data_genes_sigs12b,sigs1_2_eg2[[i]]$probe)
  #if(length(vars)>0){
  #  dat<-data2[vars,]
  #  sig_zhang<-apply(dat,2,median,na.rm=T)		
  #}
  #print(data_otherimmune2["Minterferon_Cluster_21214954",2])
  #sig<-(sig_zhang+data_otherimmune2["Minterferon_Cluster_21214954",2])/2
  #data_otherimmune2<-rbind(data_otherimmune2,sig)
  #rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]<-"GP11_Immune_IFN"
  #cat(paste(rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]),"\n")
  
  #print("DONE ZHANG")
  
  ##GP2 most correlated signatures are "KEGG_HEMATOPOEITIC_CELL_LINEAGE"   and  "REACTOME_SIGNALING_IN_IMMUNE_SYSTEM" but I couldn't find the gene list for the latter so use former
  #i=which(sigs1_2_names2=="KEGG_HEMATOPOIETIC_CELL_LINEAGE")
  #vars<-intersect(data_genes_sigs12b,sigs1_2_eg2[[i]]$probe)
  #if(length(vars)>0){
  #  dat<-data2[vars,]
  #  sig<-apply(dat,2,median,na.rm=T)
  #  data_otherimmune2<-rbind(data_otherimmune2,sig)
  #  rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]<-"GP2_ImmuneTcellBcell_score"	
    #cat(paste(rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]),"\n")		
  #}
  
  #print("DONE KEGG")
  
  #####ratios
  
  ##CD8/CD68
  
  #if((sum(is.na(data_otherimmune2["CD8A",])==FALSE)>0.1*dim(data_otherimmune2)[2]) & 
  #   (sum(is.na(data_otherimmune2["CD68",])==FALSE)>0.1*dim(data_otherimmune2)[2])){
  #  CD8<-as.numeric(data_otherimmune2["CD8A",])
  #  CD68<-as.numeric(data_otherimmune2["CD68",])
  #  CD8<-(CD8-median(CD8,na.rm=T))/sd(CD8,na.rm=T)
  #  CD68<-(CD68-median(CD68,na.rm=T))/sd(CD68,na.rm=T)
  #  ph<-abs(min(min(CD8),min(CD68)))+1
  #  CD8_CD68_ratio<-log((CD8+ph)/(CD68+ph))
  #  CD8_CD68_ratio<-CD8_CD68_ratio/sd(CD8_CD68_ratio)
  #  sig<-CD8_CD68_ratio
  #  data_otherimmune2<-rbind(data_otherimmune2,sig)
  #  rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]<-"CD8_CD68_ratio"
    #cat(paste(rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]),"\n")
  #}
  
  #print("DONE CD8 RATIO")
  
  ##TAMsurr_TcClassII_ratio
  
  #if((sum(is.na(data_otherimmune2["TAMsurr_score",])==FALSE)>0.1*dim(data_otherimmune2)[2])&(sum(is.na(data_otherimmune2["TcClassII_score",])==FALSE)>0.1*dim(data_otherimmune2)[2])){
  #  TAMsurr<-as.numeric(data_otherimmune2["TAMsurr_score",])
  #  TcClassII<-as.numeric(data_otherimmune2["TcClassII_score",])
  #  TAMsurr<-(TAMsurr-median(TAMsurr,na.rm=T))/sd(TAMsurr,na.rm=T)
  #  TcClassII<-(TcClassII-median(TcClassII,na.rm=T))/sd(TcClassII,na.rm=T)
  #  ph<-abs(min(min(TAMsurr),min(TcClassII)))+1
  #  TAMsurr_TcClassII_ratio<-log((TAMsurr+ph)/(TcClassII+ph))
  #  TAMsurr_TcClassII_ratio<-TAMsurr_TcClassII_ratio/sd(TAMsurr_TcClassII_ratio)
  #  sig<-TAMsurr_TcClassII_ratio
  #  data_otherimmune2<-rbind(data_otherimmune2,sig)
  #  rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]<-"TAMsurr_TcClassII_ratio"
    #cat(paste(rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]),"\n")
  #}
  
  #print("DONE TAMsurr")
  
  #CD103pos/CD103neg ratio
  #CD103pos<-as.numeric(data_otherimmune2["CD103pos_mean_25446897",])
  #CD103neg<-as.numeric(data_otherimmune2["CD103neg_mean_25446897",])
  #CD103pos<-(CD103pos-median(CD103pos,na.rm=T))/sd(CD103pos,na.rm=T)
  #CD103neg<-(CD103neg-median(CD103neg,na.rm=T))/sd(CD103neg,na.rm=T)
  #ph<-abs(min(min(CD103pos),min(CD103neg)))+1
  #CD103pos_CD103neg_ratio_25446897<-log((CD103pos+ph)/(CD103neg+ph))
  #CD103pos_CD103neg_ratio_25446897<-CD103pos_CD103neg_ratio_25446897/sd(CD103pos_CD103neg_ratio_25446897,na.rm=T)
  #sig<-CD103pos_CD103neg_ratio_25446897
  #data_otherimmune2<-rbind(data_otherimmune2,sig)
  #rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]<-"CD103pos_CD103neg_ratio_25446897"
  #cat(paste(rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]),"\n")
  
  ####MSIGDB CHANG_SERUM_response_up
  
  i=which(sigs1_2_names2=="CHANG_CORE_SERUM_RESPONSE_UP")
  vars<-intersect(data_genes_sigs12b,sigs1_2_eg2[[i]]$probe)
  if(length(vars)>0){
    dat<-data2[vars,]
    sig<-apply(dat,2,median,na.rm=T)
    data_otherimmune2<-rbind(data_otherimmune2,sig)
    rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]<-"CHANG_CORE_SERUM_RESPONSE_UP"
    #cat(paste(rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]),"\n")		
  }
  data_otherimmune2<-round(data_otherimmune2,digits=4)
  
  
  print("DONE CHANG CORE")
  ####correlation "CSR_Activated_15701700"
  
  
  #i=which(sigs1_2_names2=="CSR_Activated_15701700")
  #vars<-intersect(data_genes_sigs12b,sigs1_2_eg2[[i]]$probe)
  #if(length(vars)>0){
  #  dat<-data2[vars,]
  #  wt<-sigs12_weighted_means[[sigs1_2_names2[i]]][vars]
  #  dat<-wt*dat
  #  sig<-apply(dat,2,mean,na.rm=T)
  #  data_otherimmune2<-rbind(data_otherimmune2,sig)
  #  rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]<-sigs1_2_names2[i]
  #  cat(paste(rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]),"\n")
  #}
  
  
  #print("DONE CSR")
  ######5#####################################
  ####evaluate the signatures calculated as the PCA of the gene block
  
  
  for(i in doSigs){
    
    if(sigs1_2_type2[i]=="PCA"){
      vars<-intersect(data_genes_sigs12b,sigs1_2_eg2[[i]]$probes)
      if(length(vars)>3){
        dat<-data2[vars,]
        d.pca<-prcomp(~ .,data=data.frame(dat),center=F,scale=F,na.action=na.omit)$rotation[,1]
        sig<-(d.pca-mean(d.pca))/sd(d.pca)   #standardize rotation vector
        data_otherimmune2<-rbind(data_otherimmune2,sig)
        rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]<-sigs1_2_names2[i]
        ##PCA are sign-undetermined, so look to see if should flip sign (if highly anticorrelated with rest of immune genes, multiply by -1)
        datm <- as.matrix(dat)
        datm[is.infinite(datm)] <- NA
        sigmean<-apply(datm,2,mean,na.rm=T) #signature if it were mean of genes
        res0 <- cor.test(sigmean, sig)$estimate
        if(res0 < (-0.25) ) {
          data_otherimmune2[dim(data_otherimmune2)[1],]<-(-1)*data_otherimmune2[dim(data_otherimmune2)[1],]
          
        }
        cat(paste(rownames(data_otherimmune2)[dim(data_otherimmune2)[1]]),"\n")
      }
    }
  }  
  
  print("DONE PCA")
  #zscore and round
  
  #data_otherimmune2<-round(zscore.rows2(data_otherimmune2),digits=4)
  
  print("DONE DONE")
  
  return(data_otherimmune2)
}
####################END OFscript####################