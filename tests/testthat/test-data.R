context("data")

test_that("tests can build data", {
  web <- rpkgweb("data_web")

  skip_if_packages_installed(web)

  with_temp_lib(
    devtools::in_dir(
      root_dir(web),
      local({
        on.exit({
          for (p in web) {
            n <- p$package
            safe_unload(p)
          }
        }, add = TRUE)

        for (p in web) {
          n <- p$package
          expect_true(check_up(n, web, quiet = TRUE), info = n)
          safe_unload(p)
        }
      })
    )
  )
})
