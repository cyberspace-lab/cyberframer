# Add test to load the data from the "data" folder
test_that("Data loads correctly", {
  folder <- test_path("data", "md4_06-04-2022")
  exps <- load_experiments(folder)
  expect_s3_class(exps[[1]], "cyberframe")
  expect_length(exps, 2)
})
