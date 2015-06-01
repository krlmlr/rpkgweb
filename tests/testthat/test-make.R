context("make")

test_that("creation of Makefile", {
  oldwd <- setwd("test_web")
  on.exit(setwd(oldwd), add = TRUE)

  web <- read_web()

  write_makefile()
  on.exit(unlink("Makefile"), add = TRUE)

  expect_message(write_makefile(), "unchanged")

  res <- system2("make", "-n", stdout = TRUE, stderr = TRUE)
  expect_true(any(grepl("unchanged", res)))
  for (n in names(web)) {
    expect_true(any(grepl(sprintf("check_up.*%s", n), res)), label = n)
  }
})
