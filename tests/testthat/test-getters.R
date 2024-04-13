# Define a setup function to load the data
setup(function() {
  folder <- test_path("data", "md4_06-04-2022")
  exps <- load_experiments(folder)
  exp <<- exps[[1]]
  if(is.null(exp)) {
    stop("Data not loaded")
  }
})

test_that("Basic getters works", {
})
