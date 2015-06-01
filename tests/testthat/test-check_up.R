context("check_up")

test_that("check_up acceptance test", {
  lib_dir <- normalizePath(".lib", mustWork = FALSE)
  dir.create(lib_dir)
  on.exit(unlink(lib_dir, recursive = TRUE, force = TRUE), add = TRUE)

  devtools::with_lib(
    lib_dir,
    devtools::in_dir(
      "test_web",
      local({
        web <- rpkgweb()

        for (n in names(web$packages)) {
          expect_true(check_up(n, web, quiet = TRUE), info = n)
        }

        for (n in names(web$packages)) {
          expect_false(check_up(n, web, quiet = TRUE), info = n)
        }

        local({
          a_source_file <- file.path(web$packages[[1]]$path, "R", "a.R")
          writeLines("thisWillTriggerAnError", a_source_file)
          on.exit(unlink(a_source_file), add = TRUE)

          expect_message(devtools::uninstall(web$packages[[1]], quiet = TRUE),
                         lib_dir)
          expect_error(check_up(web$packages[[1]]$package, web, quiet = TRUE),
                         "thisWillTriggerAnError")

          for (n in names(web$packages)[-1:-2]) {
            expect_error(check_up(n, web, quiet = TRUE), "Command failed", info = n)
          }
        })
      })
    )
  )
})
