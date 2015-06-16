skip_if_packages_installed <- function(web) {
  # Packages are originally not installed
  if (any((web %>% names) %in% rownames(installed.packages()))) {
    skip("At least one of the test packages is installed")
  }
}

safe_unload <- function(pkg) {
  if (pkg$package %in% loadedNamespaces()) devtools::unload(pkg)
}

envvar <- function() {
  # Instruct the Makefile where to load the package from
  pkg_path <- subset(devtools::loaded_packages(), package == "rpkgweb")$path
  ret <- if (file.path(pkg_path, "tests", "testthat") == normalizePath(".", winslash = "/")) {
    # Loaded by devtools, need patched version
    list(RPKGWEB_QUALIFY = sprintf("devtools::load_all('%s');", pkg_path))
  } else {
    # Installed package in R CMD check -- reset R_TESTS which points to an invalid path
    list(R_TESTS = "")
  }
  ret <- c(ret, R_LIBS=paste(.libPaths(), collapse = ":"))
  ret
}

test_make <- function(web, target_dir = NULL, lib_dir = NULL, dry_run = FALSE) {
  skip_if_packages_installed(web)

  make_extra_commands <- NULL

  if (is.null(target_dir)) {
    makefile_path <- "Makefile"
    target_dir_local <- "."
  } else {
    makefile_path <- file.path(target_dir, "Makefile")
    make_extra_commands <- c(make_extra_commands, "-C", shQuote(target_dir))
    target_dir_local <- target_dir
  }

  if (!is.null(lib_dir)) {
    paths_to_remove <- file.path(root_dir(web), lib_dir, names(web))
    on.exit(unlink(paths_to_remove, recursive = TRUE), add = TRUE)
  }

  if (dry_run) {
    make_extra_commands <- c(make_extra_commands, "-n")
  }

  devtools::with_envvar(
    envvar(),
    devtools::in_dir(
      root_dir(web),
      local({
        write_makefile(web, target_dir = target_dir, lib_dir = lib_dir)
        expect_true(file.exists(makefile_path), info = makefile_path)
        on.exit(file.remove(makefile_path), add = TRUE)
        #writeLines(.libPaths(), "libpaths.log")

        # Early exit: If we can't load package here, something's really wrong
        # (don't recreate Makefile here)
        Sys.setFileTime(makefile_path, file.info(".")$mtime - 1L)
        res <- system2("make", c("info", make_extra_commands), stdout = TRUE, stderr = TRUE)
        #writeLines(res, "make-info.log")
        stopifnot(is.null(attr(res, "status")))

        expect_message(write_makefile(web, target_dir = target_dir, lib_dir = lib_dir),
                       "unchanged")

        Sys.setFileTime(makefile_path, file.info(".")$mtime - 1L)
        res <- system2("make", make_extra_commands, stdout = TRUE, stderr = TRUE)
        #writeLines(res, "make.log")
        expect_null(attr(res, "status"))

        expect_true(any(grepl("unchanged", res)))
        for (n in names(web)) {
          expect_true(any(grepl(sprintf("check_up.*%s", n), res)), info = n)
          if (!dry_run) {
            expect_true(any(grepl(sprintf("%s not installed", n), res)), info = n)
            expect_true(any(grepl(sprintf("%s updated", n), res)), info = n)
          }
        }

        if (!dry_run) {
          res <- system2("make", c(".rpkgweb-all-install", make_extra_commands),
                         stdout = TRUE, stderr = TRUE)
          #writeLines(res, "make-file.log")
          stopifnot(is.null(attr(res, "status")))
          expect_match(grep("^make: ", res, value = TRUE, invert = TRUE),
                       "^touch -r .* [.]rpkgweb-all-install$")
          file_time_1 <- file.info(file.path(target_dir_local, ".rpkgweb-all-install"))$mtime
          expect_true(!is.na(file_time_1))

          res <- system2("make", c(".rpkgweb-all-install", make_extra_commands),
                         stdout = TRUE, stderr = TRUE)
          #writeLines(res, "make-file-2.log")
          stopifnot(is.null(attr(res, "status")))
          expect_match(grep("^make: ", res, value = TRUE, invert = TRUE),
                       "^touch -r .* [.]rpkgweb-all-install$")
          file_time_2 <- file.info(file.path(target_dir_local, ".rpkgweb-all-install"))$mtime
          expect_true(!is.na(file_time_2))

          # Don't update file if nothing new is installed
          expect_equal(file_time_1, file_time_2)
        }
      })
    )
  )

  if (!is.null(lib_dir) || dry_run) {
    # Packages are not installed after running
    expect_false(any((web %>% names) %in% rownames(installed.packages())))
  }
}
