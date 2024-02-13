test_that("popdata validation successful", {

  # req_cols check
  expect_message(validate_popdata(test_df),
                 regexp = "^Required")

  # test if message thrown if popdf has duplicated cols
  expect_message(validate_popdata(test_df),
                 regexp = "^There are duplicated")
})
