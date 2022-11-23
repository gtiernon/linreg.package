library(linreg.package)

#import data
data("swiss")
dat = swiss
data("cars")
dat1 = cars
data("women")
dat2 = women

## The most important thing to show equality is the value of the coefficients.

# This tests that the numerical outputs of the coefficients are the same (that's why as.vector() is used).
test_that("linreg works", {
  ##Compare equality
  #dat
  expect_equal(as.vector(linreg(Fertility ~ ., dat)$Coefficients),
               as.vector(lm(Fertility ~ ., dat)[[1]]))
  expect_equal(as.vector(linreg(Infant.Mortality ~ Agriculture, dat)$Coefficients),
               as.vector(lm(Infant.Mortality ~ Agriculture, dat)[[1]]))
  expect_equal(as.vector(linreg(Education ~ Examination + Catholic, dat)$Coefficients),
               as.vector(lm(Education ~ Examination + Catholic, dat)[[1]]))
  #dat1
  expect_equal(as.vector(linreg(speed ~ dist, dat1)$Coefficients),
               as.vector(lm(speed ~ dist, dat1)[[1]]))
  #dat2
  expect_equal(as.vector(linreg(height ~ weight, dat2)$Coefficients),
               as.vector(lm(height ~ weight, dat2)[[1]]))


  ##Now to compare efficiency, use the same models
  #dat
  bench::mark(as.vector(linreg(Fertility ~ ., dat)$Coefficients),
              as.vector(lm(Fertility ~ ., dat)[[1]]))
  bench::mark(as.vector(linreg(Infant.Mortality ~ Agriculture, dat)$Coefficients),
              as.vector(lm(Infant.Mortality ~ Agriculture, dat)[[1]]))
  bench::mark(as.vector(linreg(Education ~ Examination + Catholic, dat)$Coefficients),
              as.vector(lm(Education ~ Examination + Catholic, dat)[[1]]))
  #dat1
  bench::mark(as.vector(linreg(speed ~ dist, dat1)$Coefficients),
              as.vector(lm(speed ~ dist, dat1)[[1]]))
  #dat2
  bench::mark(as.vector(linreg(height ~ weight, dat2)$Coefficients),
              as.vector(lm(height ~ weight, dat2)[[1]]))
})



