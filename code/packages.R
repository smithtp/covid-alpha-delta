silent.require <- function(x) suppressMessages(require(package=x, character.only=TRUE, quietly=TRUE))

# Load packages that are already installed
packages <- c("dplyr", "tidyr", "sf", "nlme", "nls2")

ready <- sapply(packages, silent.require)


# Install missing packages
for(i in seq_along(ready))
    if(!ready[i])
        install.packages(packages[i], quietly=TRUE, dependencies=TRUE)

# Error out if not all packages installed
ready <- sapply(packages, silent.require)
if(any(!ready))
    stop("Cannot install packages", ready[!ready])
