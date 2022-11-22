#import data
data("swiss")
dat = swiss

test_that("linreg works", {
  expect_equal(linreg(formula = Fertility ~ ., data = dat), )
})


list("Call" = "linreg(formula = Fertility ~ ., data = dat)", "Coefficients" = c("(Intercept)" = 66.9152,
          "Agriculture" = -0.1721, "Examination" = -0.258, "Education" = -0.8709, "Catholic" = 0.1041,
          "Infant.Mortality" = 1.077))
