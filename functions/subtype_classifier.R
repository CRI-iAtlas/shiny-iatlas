
# David L Gibbs
# dgibbs@systemsbiology.org

# this script will form the backend of the shiny app.


# this function computes scores given some expression data.
classifySubtype <- function(fileinfo, sepflag) {

  library(R.utils)
  library(data.table)
  library(ImmuneSubtypeClassifier)

  print(fileinfo)
  print(sepflag)

  if(is.null(fileinfo)) {
    print("HERE")
    fileinfo <- list(name='ebpp_test1_1to20.tsv',  size=1, type='text/csv', datapath='data/ebpp_test1_1to20.tsv')
  }

  print(fileinfo)

  #if (fileinfo$type == 'text/csv') {
  #  s1 <- ','
  #} else if (fileinfo$type == 'text/tab-separated-values') {
  #  s1 <- '\t'
  #} else {
  #  s1 <- '\t'
  #}
  #``

  print("Reading Data")

  #newdata <- read_csv('data/ivy20.csv')
  newdata <- read.table(file=fileinfo$datapath, sep=sepflag, header=T, stringsAsFactors = F)

  print("new data")
  print(dim(newdata))

  newX <- as.matrix(newdata[,-1])
  rownames(newX) <- as.character(newdata[,1])

  # make cluster calls using the models.
  calls <- ImmuneSubtypeClassifier::callEnsemble(X=newX, geneids='symbol')

  print(head(calls))

  calls[,3] <- round(calls[,3], digits=3)
  calls[,4] <- round(calls[,4], digits=3)
  calls[,5] <- round(calls[,5], digits=3)
  calls[,6] <- round(calls[,6], digits=3)
  calls[,7] <- round(calls[,7], digits=3)
  calls[,8] <- round(calls[,8], digits=3)

  print("Done")

  return(list(Calls=calls))

}


