test_that("%!in% basic work", {
  expect_equal(1 %!in% c(2, 1, 3), F)
  expect_equal(c(1, 5, 10) %!in% c(2,1,3), c(F, T, T))
})
