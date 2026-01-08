# Tests for identify_reference_month()

test_that("identify_reference_month validates input", {
  # Missing required columns
  bad_data <- data.frame(Ano = 2023, Trimestre = 1)
  expect_error(identify_reference_month(bad_data), "validation failed")

  # Invalid quarter
  bad_data2 <- data.frame(
    Ano = 2023, Trimestre = 5,  # Invalid quarter
    UPA = 1, V1014 = 1,
    V2008 = 15, V20081 = 6, V20082 = 1990, V2009 = 33
  )
  expect_error(identify_reference_month(bad_data2))
})

test_that("identify_reference_month returns correct structure", {
  # Create minimal valid data
  test_data <- data.frame(
    Ano = c(2023, 2023),
    Trimestre = c(1, 1),
    UPA = c(1, 1),
    V1008 = c(1, 1),
    V1014 = c(1, 1),
    V2003 = c(1, 2),
    V2008 = c(15, 20),
    V20081 = c(6, 3),
    V20082 = c(1990, 1985),
    V2009 = c(33, 38)
  )

  result <- identify_reference_month(test_data)

  # Check output columns exist
  expect_true("ref_month" %in% names(result))
  expect_true("ref_month_in_quarter" %in% names(result))
  expect_true("ref_month_yyyymm" %in% names(result))

  # Check ref_month_in_quarter values are valid
  valid_values <- result$ref_month_in_quarter[!is.na(result$ref_month_in_quarter)]
  expect_true(all(valid_values %in% 1:3))

  # Check class

  expect_s3_class(result, "pnadc_crosswalk")
})

test_that("print.pnadc_crosswalk works", {
  test_data <- data.frame(
    Ano = 2023, Trimestre = 1,
    UPA = 1, V1008 = 1, V1014 = 1, V2003 = 1,
    V2008 = 15, V20081 = 6, V20082 = 1990, V2009 = 33
  )

  result <- identify_reference_month(test_data)

  # Should not error
  expect_output(print(result), "PNADC Reference Month Crosswalk")
})
