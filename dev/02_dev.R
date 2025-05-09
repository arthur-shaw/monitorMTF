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
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "1_choose")
golem::add_module(name = "2_setup")
golem::add_module(name = "2_setup_1_suso_creds")
golem::add_module(name = "2_setup_2_qnrs")
golem::add_module(name = "2_setup_2_qnrs_hhold")
golem::add_module(name = "2_setup_2_qnrs_details")
golem::add_module(name = "3_get")
golem::add_module(name = "4_validate")
golem::add_module(name = "4_validate_0_set_status")
golem::add_module(name = "4_validate_1_validate")
golem::add_module(name = "4_validate_2_review")
golem::add_module(name = "4_validate_2_review_1_reject")
golem::add_module(name = "4_validate_2_review_1_review")
golem::add_module(name = "4_validate_2_review_1_follow_up")
golem::add_module(name = "4_validate_3_reject")
golem::add_module(name = "4_validate_4_report")
golem::add_module(name = "5_report")
golem::add_module(name = "5_report_1_completeness")
golem::add_module(name = "5_report_2_quality")

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers")
golem::add_fct(name = "data", module = "3_get")
golem::add_utils(name = "files", module = "4_validate_1_validate")
golem::add_utils(name = "files", module = "5_report")

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("monitorMTF")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
