.onAttach <- function(libname, pkgname){
    version <- read.dcf(file = system.file("DESCRIPTION", package = pkgname),
                        fields = "Version")
    email <- read.dcf(file = system.file("DESCRIPTION", package = pkgname),
                        fields = "Maintainer")
    packageStartupMessage(paste(pkgname, version),
                          " is still BETA sofrware. Please report any bugs.")
}

