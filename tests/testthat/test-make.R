context("make")

envvar <- function() {
  # Instruct the Makefile where to load the package from
  pkg_path <- subset(devtools::loaded_packages(), package == "rpkgweb")$path
  ret <- if (file.path(pkg_path, "tests", "testthat", "test_web") == normalizePath(".", winslash = "/")) {
    # Loaded by devtools, need patched version
    list(RPKGWEB_QUALIFY = sprintf("devtools::load_all('%s');", pkg_path))
  } else {
    # Installed package in R CMD check -- reset R_TESTS which points to an invalid path
    list(R_TESTS = "")
  }
  ret
}

test_that("creation of Makefile", {
  devtools::in_dir(
    "test_web",
    devtools::with_envvar(
      envvar(),
      local({
        web <- rpkgweb()

        write_makefile(web)
        on.exit(file.remove("Makefile"), add = TRUE)

        expect_message(write_makefile(), "unchanged")

        res <- system2("make", "-n", stdout = TRUE, stderr = TRUE)
        #writeLines(res, "make.log")
        expect_null(attr(res, "status"))

        expect_true(any(grepl("unchanged", res)))
        for (n in names(web$packages)) {
          expect_true(any(grepl(sprintf("check_up.*%s", n), res)), info = n)
        }
      })
    )
  )
})

test_that("execution of Makefile", {
  lib_dir <- ".lib"
  dir.create(lib_dir)
  on.exit(unlink(lib_dir, recursive = TRUE, force = TRUE), add = TRUE)
  lib_dir <- normalizePath(".lib", mustWork = TRUE)

  devtools::with_lib(
    c(lib_dir, .libPaths()),
    devtools::in_dir(
      "test_web",
      devtools::with_envvar(
        envvar(),
        local({
          stopifnot(lib_dir == .libPaths()[[1L]])
          web <- rpkgweb()

          write_makefile(web)
          on.exit(file.remove("Makefile"), add = TRUE)

          res <- system2("make", stdout = TRUE, stderr = TRUE)
          #writeLines(res, "make.log")
          expect_null(attr(res, "status"))

          expect_true(any(grepl("unchanged", res)))
          for (n in names(web$packages)) {
            expect_true(any(grepl(sprintf("check_up.*%s", n), res)), info = n)
            expect_true(any(grepl(sprintf("%s not installed", n), res)), info = n)
            expect_true(any(grepl(sprintf("%s updated", n), res)), info = n)
          }
        })
      )
    )
  )
})
