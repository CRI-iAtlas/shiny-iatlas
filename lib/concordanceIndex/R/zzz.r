.onAttach <- function(libname, pkgname){
	DBVer <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
	packageStartupMessage(
	"\n",
	paste(pkgname, DBVer), "\n",
	"\n"
	)
}


