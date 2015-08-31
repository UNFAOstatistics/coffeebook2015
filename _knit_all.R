rm(list=ls(all=TRUE)) 
gc()
library(knitr)

# set root directory
root.dir <- "~/btsync/faosync/pocketbooks/regional15/"
# set data directory
data.dir <- "~/btsync/faosync/pocketbooks/GSPB15/database/"

source(paste0(root.dir,"run.R"))
setwd("~/btsync/faosync/pocketbooks/coffeebook2015")
region_to_report <- "COF"
source(paste0(root.dir,"input/code/define_regions.R"))
source("_code/hooks_and_parameters.R")


spreads <- readr::read_csv(paste0(root.dir,"/input/define_spreads.csv"))
# subset to particular regions colunm 
spreads <- spreads[c("SPREAD",region_to_report)]

# 
for (i in 1:nrow(spreads)) {
  if (spreads[[i,2]] == 0) value <- FALSE
  if (spreads[[i,2]] == 1) value <- TRUE
  assign(spreads[[i,1]],value,envir = globalenv())
}



dirlist <- c("preface/","overview/","economy/","poverty/","des/","coffeeproduction/","coffeetrade/","coffeeprices/","countryprofiles/")

for (dir in dirlist) {
  files <- list.files(dir, pattern=".Rmd")
  files <- gsub(".Rmd","",files)
  for (i in files) {
    knit(paste0(dir,paste0(i,".Rmd")),
                paste0(dir,paste0(i,".md")))
  }
  # Kopioidaan kuvat kuhunkin kansioon (knitr hakki)
  from = "figure"
  to = paste0(dir,"figure")
  dir.create(to, recursive = TRUE)
  file.copy(list.files(from, full.names = T), to, recursive = TRUE)
}

# git add .
# git commit -am "new updates"
# git push

