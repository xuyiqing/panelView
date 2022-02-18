
setwd("~/github/panelview")

# initializing
library(usethis)
library(sinew)
library(pkgdown)
usethis::use_readme_rmd()
usethis::use_pkgdown()
usethis::use_news_md() # update logs

# remember to knitr README.Rmd
pkgdown::build_site(install = FALSE)
