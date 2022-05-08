# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
utils::install.packages( "thinkr" )
usethis::use_package( "thinkr" )

remotes::install_github("UBESP-DCTV/iobed.watch")
usethis::use_dev_package("iobed.watch")
remotes::install_github("UBESP-DCTV/iobed.bed")
usethis::use_dev_package("iobed.bed")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "name_of_module1" ) # Name of the module
golem::add_module( name = "name_of_module2" ) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "helpers" )
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE )

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()
usethis::use_github_action("test-coverage")
# Create a summary readme for the testthat subdirectory
remotes::install_github("yonicd/covrpage")
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application

# GitHub Actions
usethis::use_github()
usethis::use_github_action()
usethis::use_github_action_check_release()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

