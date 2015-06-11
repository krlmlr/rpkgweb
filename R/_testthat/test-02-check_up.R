context("check_up")

test_that("check_up acceptance test", {
  safe_unload <- function(n) {
    if (n %in% loadedNamespaces()) devtools::unload(n)
  }

  web <- rpkgweb("test_web")

  skip_if_packages_installed(web)

  with_temp_lib(

    devtools::in_dir(
      web$root_dir,
      local({
        on.exit({
          for (n in names(web$packages)) {
            safe_unload(n)
          }
        }, add = TRUE)

        for (n in names(web$packages)) {
          expect_true(check_up(n, web, quiet = TRUE), info = n)
          safe_unload(n)
        }

        for (n in names(web$packages)) {
          expect_false(check_up(n, web, quiet = TRUE), info = n)
          safe_unload(n)
        }

        local({
          a_source_file <- file.path(web$packages[[1]]$path, "R", "a.R")
          writeLines("thisWillTriggerAnError", a_source_file)
          on.exit(unlink(a_source_file), add = TRUE)

          expect_message(devtools::uninstall(web$packages[[1]], quiet = TRUE),
                         .libPaths()[[1L]])
          expect_error(check_up(web$packages[[1]]$package, web, quiet = TRUE),
                         "thisWillTriggerAnError")
          safe_unload(web$packages[[1]]$package)

          for (n in names(web$packages)[-1:-2]) {
            expect_error(check_up(n, web, quiet = TRUE), "Command failed", info = n)
          }
        })

        for (n in names(web$packages)) {
          expect_true(check_up(n, web, quiet = TRUE), info = n)
          safe_unload(n)
        }

        local({
          a_test_file <- file.path(web$packages[[1]]$path, "tests", "testthat", "test-fail.R")
          writeLines("thisWillTriggerAnError", a_test_file)
          on.exit(unlink(a_test_file), add = TRUE)

          expect_message(devtools::uninstall(web$packages[[1]], quiet = TRUE),
                         .libPaths()[[1L]])
          expect_error(check_up(web$packages[[1]]$package, web, quiet = TRUE),
                       "thisWillTriggerAnError")
          safe_unload(web$packages[[1]]$package)

          for (n in names(web$packages)[-1:-2]) {
            expect_error(check_up(n, web, quiet = TRUE), "Command failed", info = n)
          }

          writeLines(
            c(
              "context('fail')",
              "test_that('fails', { expect_true(FALSE) })"
            ),
            a_test_file)
          expect_error(check_up(web$packages[[1]]$package, web, quiet = TRUE),
                       "Test failed")
          safe_unload(web$packages[[1]]$package)
        })

        for (n in names(web$packages)) {
          expect_true(check_up(n, web, quiet = TRUE), info = n)
          safe_unload(n)
        }
      })
    )
  )
})
