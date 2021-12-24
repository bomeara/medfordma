setwd("/Users/bomeara/Documents/MyDocuments/GitClones/medfordma")
system("/usr/local/bin/git pull")
Sys.setenv(RSTUDIO_PANDOC="/usr/local/bin/pandoc")
#Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")
system("export PATH=$PATH:/usr/local/bin")

library(targets)
source("_packages.R")
source("R/functions.R")


#options(clustermq.scheduler = "multiprocess")
#tar_make_clustermq(workers = parallel::detectCores()-1)
rerun <- FALSE
if(rerun) {
	try(tar_invalidate(contains("_")))
	try(tar_invalidate(contains("sum")))
	try(tar_invalidate(contains("medford")))
}
tar_make()
Sys.setenv(RSTUDIO_PANDOC="/usr/local/bin/pandoc")
Sys.setenv(PANDOC="/usr/local/bin/pandoc")
print(rmarkdown::find_pandoc())
#to get around path issues
pandoc_available <- function(...) {
	return(TRUE)
}
rmarkdown::render_site()
Sys.sleep(10)
system("cp /Users/bomeara/Documents/MyDocuments/GitClones/EastTN/data/*csv /Users/bomeara/Documents/MyDocuments/GitClones/EastTN/docs")
#system("open docs/index.html")
system("git add docs")
system("git commit -m'updated data' -a")
system('git push')
