if (require(testthat)) {
  library(rsdk)
  test_check("rsdk")
} else {
  message("testthat not available.")
}
