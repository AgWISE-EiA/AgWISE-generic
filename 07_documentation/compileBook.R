#create dataset pages
#JRV/AG, Dec 2022

#load libraries
library(bookdown)

#working directory
wd <- "~/Repositories/AgWISE-generic/07_documentation"
setwd(wd)

#cleanup old Rmd files
rmd_list <- list.files(path=".", pattern="\\.Rmd")
rmd_list <- rmd_list[!rmd_list %in% "index.Rmd"]
for (fname in rmd_list) {system(paste0("rm -f ", fname))}

#source functions
source("createDatasetPages.R")
source("createBookdown.R")

#render book
bookdown::render_book()

#copy files into docs folder
system("cp -rvf docs ./../.")

