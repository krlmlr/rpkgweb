context("make")

test_that("creation of Makefile", {
  devtools::in_dir(
    "test_web",
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
})
