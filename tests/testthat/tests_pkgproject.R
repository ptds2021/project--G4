test_that("request_CL input check",{
  expect_error(request_CL(nasty$Date, 929))
  expect_error(request_CL("nasty", 929))
})

test_that("summary_stat input check",{
  expect_error(summary_stat(nasty$Date, 929))
  expect_error(summary_stat("nasty", 929))
})

test_that("R_bar_chart input check",{
  expect_error(R_bar_chart(nasty$Date, 929))
  expect_error(R_bar_chart("nasty", 929))
})

test_that("request_CL output",{
  expect_type(request_CL(nasty, 929), "list")
})

test_that("summary_stat output",{
  expect_type(summary_stat(nasty, 929), "list")
})

test_that("R_bar_chart output",{
  expect_type(R_bar_chart(nasty, 929), "list")
})
