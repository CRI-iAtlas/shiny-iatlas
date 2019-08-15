
# David L Gibbs
# dgibbs@systemsbiology.org

# this script will form the backend of the shiny app.


# this function computes scores given some expression data.
classifySubtype <- function(fileinfo, sepflag) {
  
  library(R.utils)
  library(data.table)
  
  print(fileinfo)
  print(sepflag)
  
  if(is.null(fileinfo)) {
    print("HERE")
    fileinfo <- list(name='ivy20.csv',  size=1, type='text/csv', datapath='data/ivy20.csv')
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
    
  # make cluster calls using the models.
  calls <- ImmuneSubtypeClassifier::callEnsemble(X=newdata, geneids='symbol')
  
  print("Done")
  
  return(calls)
  
}


