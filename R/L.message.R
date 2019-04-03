.onAttach <- function(libname, pkgname){
    version <- read.dcf(file = system.file("DESCRIPTION", package = pkgname),
                        fields = "Version")
    email <- read.dcf(file = system.file("DESCRIPTION", package = pkgname),
                        fields = "Maintainer")
    packageStartupMessage(paste(pkgname, version),
                          " is still BETA sofrware. Please report any bugs.")
    packageStartupMessage("URL: https://xf-pan.github.io/dicmo/")
    packageStartupMessage("BugReports: https://github.com/xf-pan/dicmo/issues")
    packageStartupMessage("E-mail: ", email)
}

