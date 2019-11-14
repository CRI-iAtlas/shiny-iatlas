
library(remotes)
library(BiocManager)

deps <- read.table('DEPENDENCIES', header=T, stringsAsFactors = F)

for (i in 1:nrow(deps)) 
{
  print(deps$package[i])
  installed <- require(deps$package[i], character.only = T)
  if (!installed) {
    
    if (deps$source[i] == 'CRAN') 
    {
      install.packages(deps$package[i])} 
    else if (deps$source[i] == 'github')
    {
      # this works on windows where other github packages don't
      remotes::install_github(packages = deps$package[i], ask = F)
    } 
    else 
    {
      BiocManager::install(deps$package[i])
    } 
    
  }
}

