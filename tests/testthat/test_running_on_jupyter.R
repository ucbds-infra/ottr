library(mockery)

test_that("checks for a CommManager in IRkernel", {
  mock_comm_manager <- mock(TRUE, NULL)
  stub(running_on_jupyter, "IRkernel::comm_manager", mock_comm_manager)

  expect_true(running_on_jupyter())

  expect_false(running_on_jupyter())
})
