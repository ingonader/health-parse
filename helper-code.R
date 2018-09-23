## ######################################################################### ##
## helper code
## ######################################################################### ##

# rstudioapi::restartSession()

## ============================================================================
## global variables
## ============================================================================

## ============================================================================
## build package tskeyvalparser
## ============================================================================

getwd()
## move to path of package (relative from project path):
setwd("../tskeyvalparser")
## move to path of project (relative from package path):
setwd("../health-parse")

# usethis::use_test("all")
#usethis::use_roxygen_md()
devtools::document()
devtools::test()
devtools::check()
devtools::build()
devtools::install()

## ============================================================================
## setup for package tskeyvalparser
## ============================================================================

# devtools::setup("/Users/ingonader/Dropbox/lists/health-parse/tskeyvalparser")
# usethis::use_mit_license("Ingo Nader")
# usethis::use_package("magrittr", "Suggests")
# usethis::use_package("magrittr", "Depends")
# usethis::use_package("magrittr", "Imports")  ## maybe use dplyr %>% instead?
# usethis::use_package("readr")  ## Imports
# usethis::use_package("purrr")
# usethis::use_package("stringr")
# usethis::use_package("lubridate")
# usethis::use_package("tibble")
#usethis::use_package("", "Suggests")

# ## generate package level documentation:
# usethis::use_package_doc()
# ## add this to  package level description R file:
# ## quiets concerns of R CMD check re: the .'s that appear in pipelines
# if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


