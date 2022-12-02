#create dataset pages
#JRV/AG, Dec 2022

#load libraries
library(readxl)
library(tidyverse)

#working directory
wd <- "~/Repositories/AgWISE-generic/07_documentation"
setwd(wd)

#read datafile
data <- readxl::read_xlsx("datasets.xlsx", sheet="Sheet1")
data <- data %>%
  dplyr::arrange(., Theme, Source, Variable)

#list datasets
dataset_list <- data %>%
  dplyr::select(Theme, Source, Version, Type, `Update frequency`, Title, 
                `Dataset description`, `Spatial resolution`, `Spatial coverage`,
                `Temporal resolution`, `Temporal coverage`, `Latency`, `License`, 
                `Official website`, `Sample image`) %>%
  unique(.)

#first update the 00_Datasets.md file
out_file <- file("00-Datasets.Rmd", open="w")
writeLines(text="# **Inventory of datasets**\n", con=out_file)
writeLines(text="This wiki page documents all of the datasets used by AgWISE. Below we provide a table that summarizes all datasets, and within the table there is a link that documents clearly each dataset and the variables that compose it.\n", con=out_file)
writeLines(text="| Theme               | Source               | Version | Type               | Update freq. |
|---------------------|----------------------|---------|--------------------|--------------|", con=out_file)
for (i in 1:nrow(dataset_list)) {
  writeLines(text=paste0("| ", dataset_list$Theme[i], " | ", 
                         "[", dataset_list$Source[i], "][", dataset_list$Title[i],"] | ",
                         dataset_list$Version[i], " | ",
                         dataset_list$Type[i], " | ", 
                         dataset_list$`Update frequency`[i], " | "), 
             con=out_file)
}
writeLines(text="\n", con=out_file)
close(con=out_file)

#now update individual datasets
for (i in 1:nrow(dataset_list)) {
  #i <- 1
  #filter for variables in dataset and select variables
  dset_vars <- data %>%
    dplyr::filter(., Source == paste(dataset_list$Source[i])) %>%
    dplyr::select(Variable:Units)
  out_file <- file(paste0(sprintf("%02.0f",i),"-", dataset_list$Source[i],".Rmd"), open="w")
  #writeLines(text="# Weather and climate", con=out_file)
  
  #write dataset level information
  writeLines(text=paste0("## ", dataset_list$Title[i], "{.unnumbered}\n"), con=out_file)
  writeLines(text=paste0(dataset_list$`Dataset description`[i], "\n\n"), con=out_file)
  writeLines(text="### Dataset characteristics {.unlisted .unnumbered}", con=out_file)
  writeLines(text=paste0("-   Spatial resolution: ", dataset_list$`Spatial resolution`[i]), con=out_file)
  writeLines(text=paste0("-   Spatial coverage: ", dataset_list$`Spatial coverage`[i]), con=out_file)
  writeLines(text=paste0("-   Temporal resolution: ", dataset_list$`Temporal resolution`[i]), con=out_file)
  writeLines(text=paste0("-   Temporal coverage:", dataset_list$`Temporal coverage`[i]), con=out_file)
  writeLines(text=paste0("-   Update frequency / latency: ", dataset_list$Latency[i]), con=out_file)
  writeLines(text=paste0("-   Version: ", dataset_list$Version[i]), con=out_file)
  writeLines(text=paste0("-   License: ", dataset_list$License[i]), con=out_file)
  writeLines(text=paste0("-   Official website: <", dataset_list$`Official website`[i], ">"), con=out_file)
  writeLines(text="\n", con=out_file)
  
  #write variable table
  writeLines(text="### Variables included {.unlisted .unnumbered}\n", con=out_file)
  writeLines(text="| Variable name | Description                                              | Temporal resolution | Units  |", con=out_file)
  writeLines(text="|---------------|-----------------------------|---------------|---------------|", con=out_file)
  for (j in 1:nrow(dset_vars)) {
    writeLines(text=paste0("| ", dset_vars$Variable[j], 
                           " | ", dset_vars$`Variable description`[j],
                           " | ", dset_vars$`Variable temporal resolution`[j],
                           " | ", dset_vars$Units[j]), con=out_file)
  }
  writeLines(text="\n", con=out_file)
  
  #write image if not N/A and if file exists
  dataset_list <- dataset_list %>%
    dplyr::mutate(samp_img=as.factor(`Sample image`))
  if (!is.na(dataset_list$samp_img[i])) {
    if (file.exists(paste0("images/", dataset_list$samp_img[i]))) {
      writeLines(text="### Sample image of the dataset {.unlisted .unnumbered}\n", con=out_file)
      writeLines(text=paste0("![](images/",dataset_list$samp_img[i], ")\n"), con=out_file)
    }
  }
  writeLines(text="\n", con=out_file)
  close(con=out_file)
}




