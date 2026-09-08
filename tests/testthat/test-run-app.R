test_that("the default output folder uses the here project root", {
  expect_identical(
    micsPlusTableR:::default_output_dir(),
    here::here("micsPlusTableR-output")
  )
})
