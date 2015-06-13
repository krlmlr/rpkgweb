context("make")

test_that("creation of Makefile", {
  web <- rpkgweb("test_web")

  skip_if_packages_installed(web)

  devtools::in_dir(
    root_dir(web),
    devtools::with_envvar(
      envvar(),
      local({
        write_makefile(web)
        on.exit(file.remove("Makefile"), add = TRUE)

        expect_message(write_makefile(), "unchanged")

        res <- system2("make", "-n", stdout = TRUE, stderr = TRUE)
        #writeLines(res, "make.log")
        expect_null(attr(res, "status"))

        expect_true(any(grepl("unchanged", res)))
        for (n in names(web)) {
          expect_true(any(grepl(sprintf("check_up.*%s", n), res)), info = n)
        }
      })
    )
  )
})

test_that("execution of Makefile", {
  web <- rpkgweb("test_web")

  skip_if_packages_installed(web)

  with_temp_lib(
    devtools::in_dir(
      root_dir(web),
      devtools::with_envvar(
        envvar(),
        local({
          write_makefile(web)
          on.exit(file.remove("Makefile"), add = TRUE)

          res <- system2("make", stdout = TRUE, stderr = TRUE)
          #writeLines(res, "make.log")
          expect_null(attr(res, "status"))

          expect_true(any(grepl("unchanged", res)))
          for (n in names(web)) {
            expect_true(any(grepl(sprintf("check_up.*%s", n), res)), info = n)
            expect_true(any(grepl(sprintf("%s not installed", n), res)), info = n)
            expect_true(any(grepl(sprintf("%s updated", n), res)), info = n)
          }
        })
      )
    )
  )

  # Packages are not installed after running
  expect_false(any((web %>% names) %in% rownames(installed.packages())))
})
