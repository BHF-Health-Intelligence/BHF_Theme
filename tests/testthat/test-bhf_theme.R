test_that("bhf_theme returns a ggplot2 theme", {
  thm <- bhf_theme()
  expect_s3_class(thm, "theme")
})
