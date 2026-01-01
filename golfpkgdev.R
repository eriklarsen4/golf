# golf pkg creation ----
usethis::create_package(path = 'C:/Users/Erik/Desktop/Programming/R/Sports/golf', roxygen = TRUE)

# golf pkg build/install ----
devtools::document(pkg = 'C:/Users/Erik/Desktop/Programming/R/Sports/golf')
devtools::build(pkg = 'C:/Users/Erik/Desktop/Programming/R/Sports/golf', path = 'C:/Users/Erik/Desktop/Programming/R/Sports/golf', binary = T, vignettes = F)

# one-time re-set of the internal clock; discrepancy causes note generation in pkg build check
#usethis::edit_r_environ()
# usethis::use_gpl3_license()

devtools::check(pkg = 'C:/Users/Erik/Desktop/Programming/R/Sports/golf', document = T, vignettes = F, cran = T)
devtools::load_all(export_all = T)

devtools::uninstall('C:/Users/Erik/Desktop/Programming/R/Sports/golf',unload = T, quiet = T)

devtools::install('C:/Users/Erik/Desktop/Programming/R/Sports/golf', reload = T, build = T, dependencies = T)
devtools::install('C:/Users/Erik/Desktop/Programming/R/Sports/golf', reload = T, build = F, dependencies = T)


roxygen2::roxygenize(clean = TRUE, package.dir = 'C:/Users/Erik/Desktop/Programming/R/Sports/golf')
# pkgbuild::check_build_tools(debug = TRUE)

# TMEM pkg use news MD ----
usethis::use_news_md()
# TMEM pkg github standard check (builds R CMD check workflow)----

library(testthat)
usethis::use_github_action("check-standard")
biocthis::use_bioc_github_action()
install.packages("covr")
library(covr)

usethis::use_news_md()
usethis::use_vignette("golf")

# golf pkg test that ----
testthat::test_that("golf")
usethis::use_testthat()
usethis::use_test()

# golf pkg data load ----


# check and release golf pkg ----
#sign in
rhub::validate_email()
# check
devtools::check_rhub()

devtools::check_win_devel()

cran_checks <- rhub::check_for_cran()

#check package viability on all OSs
devtools::check_win_devel()

results$cran_summary()

devtools::test_coverage()

#release to cran
devtools::release()
devtools::spell_check(pkg = 'C:/Users/Erik/Desktop/Programming/R/Sports/golf', vignettes = TRUE, use_wordlist = TRUE)


##
gitcreds::gitcreds_set(url = 'https://www.github.com/eriklarsen4/golf')
rhub::rhub_check(gh_url = 'https://www.github.com/eriklarsen4/golf')
usethis::create_github_token()

usethis::use_spell_check(vignettes = TRUE, lang = 'en-US', error = FALSE)

# misc ----


# ----
spelling::update_wordlist()