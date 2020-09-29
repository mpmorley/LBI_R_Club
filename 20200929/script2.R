library(tidyverse)

#Create a vector of dirs we wish to make
dirs <- c('bin','data','plots','results')

#Print to screen which directory we are currenlty in. 
print(paste0('Current working directory is ',getwd()))


#Create a function that makes a directory
mkdir <- function(dir){
  print(paste0('Creating ',dir,' Directory'))
  dir.create(dir,showWarnings = F) 
}

map(dirs,mkdir)

print('Project directory is now setup')
