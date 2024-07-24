test_that("Units and names convert and values are reported correctly", {
  # How report values?
  expect_equal(2 * 2, 4)
})


# Test that analytes (Fraction, ANALYTe, etc.) appear in map tab
#   for each respective study
# Test that each code name appears in Key tab
# Test that each unique reported units/ target units combo
#   appears in unit conversion tab
# Add impute column (which also flags if it's already estimated) for specified Remarks
#   Add DL column, then test if want to impute DL exists
# Could add options for the imputation method
# Give Eric's table priority over < values in GLENDA
#
