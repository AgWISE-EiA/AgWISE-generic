#create bookdown.yml file
#JRV/AG, Dec 2022

#working directory
wd <- "~/Repositories/AgWISE-generic/07_documentation"
setwd(wd)

#list of Rmd files
rmd_list <- list.files(path=".", pattern="\\.Rmd")
rmd_list <- rmd_list[!rmd_list %in% "index.Rmd"]

#create bookdown file
out_file <- file("_bookdown.yml", open="w")
writeLines(text="book_filename: agwise", con=out_file)
writeLines(text="rmd_files: ", con=out_file)
writeLines(text='  - "index.Rmd"', con=out_file)
for (i in 1:length(rmd_list)) {writeLines(text=paste0('  - "', rmd_list[i], '"'), con=out_file)}
writeLines(text="new_session: yes", con=out_file)
writeLines(text="delete_merged_file: true", con=out_file)
writeLines(text='output_dir: "docs"', con=out_file)
writeLines(text="language:", con=out_file)
writeLines(text="  label:", con=out_file)
writeLines(text='    fig: "FIGURE "', con=out_file)
writeLines(text='    tab: "TABLE "', con=out_file)
writeLines(text='  ui:', con=out_file)
writeLines(text='    edit: "Edit"', con=out_file)
writeLines(text='    chapter_name: "Section "', con=out_file)
close(out_file)
