context("check_up")

local({
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
          expect_message(check_up(n, web), "Package .* updated", label = n)
        }
      })
    )
  )
})
