test_that("signif_sym() basic work", {
  expect_equal(signif_sym(3e-4), "***")
})
