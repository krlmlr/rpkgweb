context("make")

test_that("creation of Makefile", {
  web <- rpkgweb("test_web")

  skip_if_packages_installed(web)

  devtools::with_envvar(
    envvar(),
    devtools::in_dir(
      root_dir(web),
      local({
        for (target_dir in list(NULL, ".", "unrelated")) {
          write_makefile(web, target_dir = target_dir)

          if (is.null(target_dir)) {
            makefile_path <- "Makefile"
            make_extra_commands <- NULL
          } else {
            makefile_path <- file.path(target_dir, "Makefile")
            make_extra_commands <- c("-C", shQuote(target_dir))
          }

          expect_true(file.exists(makefile_path), info = makefile_path)
          on.exit(file.remove(makefile_path), add = TRUE)

          expect_message(write_makefile(web, target_dir = target_dir), "unchanged")

          res <- system2("make", c("-n", make_extra_commands), stdout = TRUE, stderr = TRUE)
          #writeLines(res, "make.log")
          expect_null(attr(res, "status"),
                      info = paste(make_extra_commands, collapse = " "))

          expect_true(any(grepl("unchanged", res)))
          for (n in names(web)) {
            expect_true(any(grepl(sprintf("check_up.*%s", n), res)), info = n)
          }
        }
      })
    )
  )
})

test_that("execution of Makefile for temp lib", {
  web <- rpkgweb("test_web")

  skip_if_packages_installed(web)

  with_temp_lib(test_make(web))

  # Packages are not installed after running
  expect_false(any((web %>% names) %in% rownames(installed.packages())))
})

test_that("execution of Makefile for other lib", {
  web <- rpkgweb("test_web")

  skip_if_packages_installed(web)

  test_make(web, lib_dir = "unrelated")

  # Packages are not installed after running
  expect_false(any((web %>% names) %in% rownames(installed.packages())))
})
