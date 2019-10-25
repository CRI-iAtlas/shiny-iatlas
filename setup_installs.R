# library(BiocManager)

deps <- read.table('DEPENDENCIES', header=T, stringsAsFactors = F)

alreadyInstalled <- 0
installedFromGithub <- 0
installedFromCran <- 0
installedFromBioc <- 0

for (i in 1:nrow(deps)) {
  packageName <- deps$package[i]
  source <- deps$source[i]
  if (source == 'github') {
    githubPath <- packageName

    packageName <- strsplit(githubPath,"/")[[1]]
    packageName <- packageName[length(packageName)]
  }
  installed <- require(packageName, character.only = T)
  if (installed) {
    cat("installed: ", packageName, "\n")
    alreadyInstalled <- alreadyInstalled + 1
  } else {
    cat("installing: ", packageName, "\n")
    if (source == 'CRAN') {
      install.packages(packageName)
      installedFromCran <- installedFromCran + 1

    } else if (source == 'github') {
      devtools::install_github(githubPath, ask = F)
      installedFromGithub <- installedFromGithub + 1
    } else {
      library(BiocManager)
      BiocManager::install(packageName)
      installedFromBioc <- installedFromBioc + 1
    }
  }
}

cat("alreadyInstalled: ", alreadyInstalled, "\n")
cat("installedFromGithub: ", installedFromGithub, "\n")
cat("installedFromCran: ", installedFromCran, "\n")
cat("installedFromBioc: ", installedFromBioc, "\n")
