


getSubtypeTable <- function() {
  read.table('data/five_signature_mclust_ensemble_results.tsv.gz', sep='\t', header=T, stringsAsFactors = F)
}

tablef <- function(a,b) {
  table(a,b)
}
