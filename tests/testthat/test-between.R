
test_that("check_date outputs expected dates: ", {
  dates <<- check_dates(ReportStart, ReportEnd)
  expect_equal(dates, vctrs::vec_c(start = lubridate::ymd("2020-01-25"), end = lubridate::ymd("2020-08-30"), .ptype = Sys.Date()))
  expect_equal(check_dates("01012020", "02022020"), vctrs::vec_c(start = lubridate::ymd("2020-01-01"), end = lubridate::ymd("2020-02-02"), .ptype = Sys.Date()))
})
ReportStart <- dates[[1]]
ReportEnd <- dates[[2]]
test_that("stayed_between filters properly", {
  .result <- stayed_between(test_data$stayed, ReportStart, ReportEnd)
  expect_true(range(.result$ExitDate, na.rm = TRUE)[1] > dates["start"])
  expect_true(range(.result$EntryAdjust, na.rm = TRUE)[2] <= dates["end"])
})

test_that("served_between filters properly", {
  .result <- served_between(test_data$served, ReportStart, ReportEnd)
  expect_true(range(.result$ExitDate, na.rm = TRUE)[1] > dates["start"])
  expect_true(range(.result$EntryDate, na.rm = TRUE)[2] <= dates["end"])
})

test_that("entered_between filters properly", {
  .result <- entered_between(test_data$entered, ReportStart, ReportEnd)
  .r <- range(.result$EntryDate, na.rm = TRUE)
  expect_true(all(.r >= dates["start"] & .r <= dates["end"]))
})

test_that("exited_between filters properly", {
  .result <- exited_between(test_data$exited, ReportStart, ReportEnd)
  .r <- range(.result$ExitDate, na.rm = TRUE)
  expect_true(all(.r >= dates["start"] & .r <= dates["end"]))
})

test_that("operating_between filters properly", {
  .result <- operating_between(test_data$operating, ReportStart, ReportEnd)
  expect_false(any(is.na(.result$OperatingStartDate))) 
  .r <- range(.result$OperatingStartDate)
  expect_true(all(.r <= dates["end"]))
  expect_true(all(.result$OperatingEndDate >= dates["start"] | is.na(.result$OperatingEndDate)))
})

test_that("beds_available_between filters properly", {
  .result <- beds_available_between(test_data$beds_available, ReportStart, ReportEnd)
  expect_false(any(is.na(.result$InventoryStartDate))) 
  .r <- range(.result$InventoryStartDate)
  expect_true(all(.r <= dates["end"]))
  expect_true(all(.result$InventoryEndDate >= dates["start"] | is.na(.result$InventoryEndDate)))
})
