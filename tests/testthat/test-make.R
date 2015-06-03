context("make")

test_that("creation of Makefile", {
  pkg_path <- subset(devtools::loaded_packages(), package == "rpkgweb")$path
  devtools::in_dir(
    "test_web",
    devtools::with_envvar(
      # Use load_all() instead of qualification :: to access test version
      # of package instead of installed version
      list(RPKGWEB_QUALIFY = sprintf("devtools::load_all('%s'); ", pkg_path)),
      local({
        web <- rpkgweb()

        write_makefile(web)
        on.exit(file.remove("Makefile"), add = TRUE)

        expect_message(write_makefile(), "unchanged")

        res <- system2("make", "-n", stdout = TRUE, stderr = TRUE)
        expect_null(attr(res, "status"))

        expect_true(any(grepl("unchanged", res)))
        for (n in names(web$packages)) {
          expect_true(any(grepl(sprintf("check_up.*%s", n), res)), info = n)
        }
      })
    )
  )
})
