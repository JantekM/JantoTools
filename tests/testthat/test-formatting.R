test_that("signif_sym() basic work", {
  expect_equal(signif_sym(3e-4), "***")
  expect_equal(signif_sym(c(0.05, 0, 1, 0.1)), c("*", "*****", " ", "."))
  expect_error(signif_sym(-1))
  expect_error(signif_sym(2))
  expect_equal(signif_sym(c(1, 0.5, NA, 0.03, 0.008, 0.0008, NA)), c(" "," ","","*","**","***",""))
})
test_that("signif_sym() custom params", {
  expect_equal(signif_sym(3e-10, max_asterisks = 5), "*****")
  expect_equal(signif_sym(3e-10, max_asterisks = 6), "******")
  expect_equal(signif_sym(3e-10, max_asterisks = 2), "**")
  expect_error(signif_sym(3e-10, max_asterisks = 1))

  expect_equal(signif_sym(c(1, 0.5, 0.07, 0.03, 0.008, 0.0008, 0.00008), bonferoni = 1), c(" "," ",".","*","**","***","****"))
  expect_equal(signif_sym(c(1, 0.5, 0.07, 0.03, 0.008, 0.0008, 0.00008), bonferoni = 4), c(" "," ","*","**","**","***","****"))
  expect_equal(signif_sym(c(1, 0.5, 0.07, 0.03, 0.008, 0.0008, 0.00008), bonferoni = 14), c(".","*","**","**","***","****","*****"))

  expect_equal(signif_sym(c(1, 0.5, 0.07, 0.03, 0.008, 0.0008, 0.00008), include_dot = F), c(" "," "," ","*","**","***","****"))
  expect_equal(signif_sym(c(1, 0.5, 0.07, 0.03, 0.008, 0.0008, 0.00008), include_dot = F, ns_sym = 'ns'), c("ns","ns","ns","*","**","***","****"))
})


test_that("remove_diacritics() basic work", {
  expect_equal(remove_diacritics("Abece \u017C\u00F3\u0142\u0107"), "Abece zolc")
  expect_equal(remove_diacritics(c("Abece", "\u017C\u00F3\u0142\u0107", "lata \u0107ma")), c("Abece", "zolc", "lata cma"))
})

test_that("formatP() basic work", {
  expect_equal(formatP(0.0026485635335), "0.265%")
  expect_equal(formatP(0.0026485635335, sig.digs = 2), "0.26%")
  expect_equal(formatP(5e-33), "<0.001%")
  expect_equal(formatP(1), ">99.99%")
  expect_equal(formatP(), "infinity")
})

test_that("formatOR() basic work", {
  expect_equal(formatOR(0.0026485635335), "0.00265")
  expect_equal(formatOR(0.0026485635335, sig.digs = 2), "0.0026")
  expect_equal(formatOR(Inf), "Inf")
  expect_equal(formatOR(Inf, inf.as = 'infinity'), "infinity")
})
