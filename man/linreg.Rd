\name{linreg}
\alias{linreg}
\title{linreg}
\description{
Fits a linear regression model to a dataset and outputs coefficients.
}
\references{
GitHub link:
\url{https://github.com/gtiernon/linreg.package}
}
\author{Grace Tiernon}
\usage{
linreg(formula, data)
}
\arguments{
\item{formula}{An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
\item{data}{A data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
}
\seealso{
Comparable to the lm() function.
\url{https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm}
}
\examples{
## import data
data("swiss")
dat = swiss

## call function
linreg(formula = Fertility ~ Education, data = dat)
linreg(formula = Agriculture ~ ., data = dat)
linreg(Fertility ~ Education + Agriculture + Examination + Catholic + Infant.Mortality, dat)
}
