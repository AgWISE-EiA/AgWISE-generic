#create dataset pages
#JRV/AG, Dec 2022

#load libraries
library(bookdown)

#working directory
wd <- "~/Repositories/AgWISE-generic/07_documentation"
setwd(wd)

#render book
bookdown::render_book()

#copy files into docs folder
system("cp -rvf docs ./../.")
