context("corner")

test_that("empty web", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)

  web <- rpkgweb(tmp_dir)
  expect_equal(length(web), 0)
  expect_match(format(web), "without any packages")

  devtools::with_envvar(
    envvar(),
    devtools::in_dir(
      root_dir(web),
      local({
        write_makefile(web)

        expect_message(write_makefile(web), "unchanged")

        res <- system2("make", "-n", stdout = TRUE, stderr = TRUE)
        expect_null(attr(res, "status"))
      })
    )
  )
})
