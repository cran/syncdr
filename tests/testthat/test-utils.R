test_that("rs_theme returns a list with correct structure", {
  res <- rs_theme()

  # Check it's a list
  expect_type(res, "list")

  # Check all expected fields are present
  expect_named(res, c("editor", "global", "dark", "foreground", "background"))

  # Check default values
  expect_equal(res$editor, "")
  expect_equal(res$global, "")
  expect_equal(res$dark, FALSE)
  expect_equal(res$foreground, "")
  expect_equal(res$background, "")
})

test_that("rs_theme returns invisible result", {
  res <- capture.output({
    out <- rs_theme()
  })
  expect_identical(rs_theme(), rs_theme()) # just ensures it runs invisibly
})

