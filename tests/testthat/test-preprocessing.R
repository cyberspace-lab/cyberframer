test_that("extraction of position column works", {
  df <- data.frame(positions = c("(5.334, 5.411, 0.000)", "(5.334, 5.411, 0.000)"))
  pos <- stringr::str_match(df$positions, "\\((.*),(.*),(.*)\\)")[, 2:4]
  stringr::str_trim(pos)
})
