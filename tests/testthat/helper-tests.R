ReportStart <- "20200125"
ReportEnd <- "30082020"
message(.Library)
message(.libPaths())
if (testthat::is_testing()) {
  .data_path <- fs::path("test-between", ext = "rds")
} else {
  .data_path <- here::here("tests", "testthat", "test-between.rds")
}

dates <- c()
test_data <- readRDS(.data_path)