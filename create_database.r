#### Script for collecting R help function text.
# Not needed for web app functionality, but left in case.

library(rvest)
pkglist <- read.table(file = "packagelist.txt", sep = ",", header = TRUE)   
fnclist <- getNamespaceExports(pkglist$package[i])

("dplyr")

i<-1; j <-117

path <- paste0(pkglist$url[i], "/topics/",fnclist[j])

rvest()
??base
