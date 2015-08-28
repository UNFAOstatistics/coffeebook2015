
library(knitr)

dirlist <- c("preface/","overview/","economy/","poverty/","des/","coffee_production","coffee_trade","coffee_prices","country_profiles")

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

