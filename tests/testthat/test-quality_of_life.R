test_that("%notin% basic work", {
  expect_equal(1 %notin% c(2, 1, 3), F)
  expect_equal(c(1, 5, 10) %notin% c(2,1,3), c(F, T, T))
})
