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

        on.exit({
          for (n in names(web$packages)) {
            if (n %in% loadedNamespaces()) devtools::unload(n)
          }
        }, add = TRUE)

        for (n in names(web$packages)) {
          expect_true(check_up(n, web, quiet = TRUE), info = n)
          if (n %in% loadedNamespaces()) devtools::unload(n)
        }

        for (n in names(web$packages)) {
          expect_false(check_up(n, web, quiet = TRUE), info = n)
          if (n %in% loadedNamespaces()) devtools::unload(n)
        }

        expect_message(devtools::uninstall(web$packages[[1]], quiet = TRUE),
                       lib_dir)
        with_mock(
          `devtools::install` = function(x, ...) stop("devtools::install"),
          expect_error(check_up(web$packages[[1]]$package, web, quiet = TRUE),
                       "devtools::install")
        )

        devtools::unload(web$packages[[1]]$package)

        with_mock(
          `devtools::test` = function(pkg, reporter, ...) {
            expect_equal(reporter, "stop")
            stop("devtools::test")
          },
          expect_error(check_up(web$packages[[1]]$package, web, quiet = TRUE),
                       "devtools::test")
        )

        # No unloading here
        #devtools::unload(web$packages[[1]]$package)

        for (n in names(web$packages)[-1:-2]) {
          expect_error(check_up(n, web, quiet = TRUE), "Command failed", info = n)
        }

        for (n in names(web$packages)) {
          expect_true(check_up(n, web, quiet = TRUE), info = n)
          if (n %in% loadedNamespaces()) devtools::unload(n)
        }
      })
    )
  )
})
