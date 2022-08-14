
setwd("~/github/panelView")

# initializing
library(usethis)
library(sinew)
library(pkgdown)
usethis::use_readme_rmd()
usethis::use_pkgdown()
usethis::use_news_md() # update logs

# remember to knitr README.Rmd
library(pkgdown)
build_site(install = FALSE)
build_home()
build_reference()
build_articles()
build_tutorials()
build_news()

