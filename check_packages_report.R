#function to check if required packages have been installed
check_packages = function(names){
  for(name in names){
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org",dependencies=TRUE) #if package not installed, install the package
    
    library(name, character.only=TRUE)
  }
}
# Checks to see if required packages are already installed.
check_packages(c("ggplot2","tidyverse", "corrplot"))  #check these packages


# install git packages: gganimate
devtools::install_github('thomasp85/gganimate', dependencies = TRUE)


