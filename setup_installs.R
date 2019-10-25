library(githubinstall)
library(BiocManager)

deps <- read.table('DEPENDENCIES', header=T, stringsAsFactors = F)

for (i in 1:nrow(deps)) {
  installed <- require(deps$package[i], character.only = T)
  if (!installed) {
    if (deps$source[i] == 'CRAN') {
      install.packages(deps$package[i])

    } else if (deps$source[i] == 'github') {
      devtools::install_github(deps$package[i], ask = F)

    } else {
      BiocManager::install(deps$package[i])
    }
  }
}
